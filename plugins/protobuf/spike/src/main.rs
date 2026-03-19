/// Spike: verify protobuf-parse + prost-reflect two-crate approach
///
/// Tests:
/// 1. Dependency compatibility (just compiling proves this)
/// 2. Serialization bridge: protobuf-parse → bytes → prost-reflect pool
/// 3. protobuf-parse input API (in-memory vs file-on-disk)
/// 4. Import resolution with include paths
/// 5. DynamicMessage round-trip encode/decode
use std::io::Write;

use prost::Message as ProstMessage;
use prost_reflect::{DescriptorPool, DynamicMessage, Value as PbValue};
use protobuf::Message as ProtobufMessage;

const TEST_PROTO: &str = r#"
syntax = "proto3";
package test;

enum Status {
  UNKNOWN = 0;
  OK = 1;
  ERROR = 2;
}

message Person {
  string name = 1;
  int32 age = 2;
  repeated string tags = 3;
  Status status = 4;
  map<string, int32> scores = 5;
}

message Team {
  string team_name = 1;
  repeated Person members = 2;
}
"#;

/// Parse a .proto string via temp file, return DescriptorPool.
///
/// Uses parse_and_typecheck() (not file_descriptor_set()) to include all
/// transitive dependencies in the resulting pool.
fn parse_proto_via_tempfile(proto_src: &str, virtual_name: &str) -> anyhow::Result<DescriptorPool> {
    let dir = tempfile::tempdir()?;
    let proto_path = dir.path().join(virtual_name);
    {
        let mut f = std::fs::File::create(&proto_path)?;
        f.write_all(proto_src.as_bytes())?;
    }

    // Use parse_and_typecheck() to get all files (including deps),
    // then build our own FileDescriptorSet from the full list.
    // file_descriptor_set() strips deps — not usable for multi-file imports.
    let parsed = protobuf_parse::Parser::new()
        .pure()
        .include(dir.path())
        .input(&proto_path)
        .parse_and_typecheck()?;

    let mut fds = protobuf::descriptor::FileDescriptorSet::new();
    fds.file = parsed.file_descriptors;

    // Serialization bridge: protobuf crate → bytes → prost-reflect
    let bytes = fds.write_to_bytes()?;
    let pool = DescriptorPool::decode(bytes.as_slice())?;
    Ok(pool)
}

fn test_basic_bridge() -> anyhow::Result<()> {
    println!("=== Test 1: basic bridge (single .proto) ===");

    let pool = parse_proto_via_tempfile(TEST_PROTO, "test.proto")?;

    let person_desc = pool
        .get_message_by_name("test.Person")
        .ok_or_else(|| anyhow::anyhow!("test.Person not found in pool"))?;
    println!("  Found message: {}", person_desc.full_name());

    let team_desc = pool
        .get_message_by_name("test.Team")
        .ok_or_else(|| anyhow::anyhow!("test.Team not found in pool"))?;
    println!("  Found message: {}", team_desc.full_name());

    // Verify field count (Person has 5 user fields; scores map generates an
    // internal MapEntry message so all_messages may have more, but Person
    // itself has 5 fields as defined)
    let person_fields: Vec<_> = person_desc.fields().collect();
    assert_eq!(
        person_fields.len(),
        5,
        "Person should have 5 fields, got {}",
        person_fields.len()
    );
    println!(
        "  Person fields: {:?}",
        person_fields.iter().map(|f| f.name()).collect::<Vec<_>>()
    );

    println!("  PASS");
    Ok(())
}

fn test_roundtrip() -> anyhow::Result<()> {
    println!("=== Test 2: DynamicMessage encode/decode round-trip ===");

    let pool = parse_proto_via_tempfile(TEST_PROTO, "test.proto")?;
    let person_desc = pool.get_message_by_name("test.Person").unwrap();

    // Build a DynamicMessage for Person
    let mut msg = DynamicMessage::new(person_desc.clone());

    let name_field = person_desc.get_field_by_name("name").unwrap();
    let age_field = person_desc.get_field_by_name("age").unwrap();
    let tags_field = person_desc.get_field_by_name("tags").unwrap();
    let status_field = person_desc.get_field_by_name("status").unwrap();

    msg.set_field(&name_field, PbValue::String("Alice".to_string()));
    msg.set_field(&age_field, PbValue::I32(30));
    msg.set_field(
        &tags_field,
        PbValue::List(vec![
            PbValue::String("dev".to_string()),
            PbValue::String("lisp".to_string()),
        ]),
    );
    // Status enum: value 1 = OK
    msg.set_field(&status_field, PbValue::EnumNumber(1));

    // Encode to bytes
    let mut encoded = Vec::new();
    msg.encode(&mut encoded)?;
    println!("  Encoded {} bytes", encoded.len());
    assert!(!encoded.is_empty(), "encoded bytes should not be empty");

    // Decode back
    let decoded = DynamicMessage::decode(person_desc.clone(), encoded.as_slice())?;

    // Verify fields
    let got_name = decoded.get_field_by_name("name").unwrap();
    assert_eq!(
        got_name.as_ref(),
        &PbValue::String("Alice".to_string()),
        "name mismatch"
    );

    let got_age = decoded.get_field_by_name("age").unwrap();
    assert_eq!(got_age.as_ref(), &PbValue::I32(30), "age mismatch");

    let got_tags = decoded.get_field_by_name("tags").unwrap();
    match got_tags.as_ref() {
        PbValue::List(items) => {
            assert_eq!(items.len(), 2, "tags length mismatch");
            assert_eq!(items[0], PbValue::String("dev".to_string()));
            assert_eq!(items[1], PbValue::String("lisp".to_string()));
        }
        other => panic!("tags should be List, got {:?}", other),
    }

    let got_status = decoded.get_field_by_name("status").unwrap();
    assert_eq!(
        got_status.as_ref(),
        &PbValue::EnumNumber(1),
        "status mismatch"
    );

    println!(
        "  Round-trip verified: name={:?}, age={:?}",
        got_name, got_age
    );
    println!("  PASS");
    Ok(())
}

const IMPORTED_PROTO: &str = r#"
syntax = "proto3";
package test;

message Address {
  string street = 1;
  string city = 2;
}
"#;

const IMPORTING_PROTO: &str = r#"
syntax = "proto3";
package test;

import "address.proto";

message Contact {
  string name = 1;
  Address home = 2;
}
"#;

fn test_import_resolution() -> anyhow::Result<()> {
    println!("=== Test 3: import resolution ===");

    let dir = tempfile::tempdir()?;

    // Write address.proto (the dependency)
    std::fs::write(dir.path().join("address.proto"), IMPORTED_PROTO)?;
    // Write contact.proto (the main file that imports address.proto)
    let contact_path = dir.path().join("contact.proto");
    std::fs::write(&contact_path, IMPORTING_PROTO)?;

    // parse_and_typecheck returns ALL files (input + deps), unlike
    // file_descriptor_set() which strips deps. We need all of them
    // so prost-reflect can resolve the cross-file references.
    let parsed = protobuf_parse::Parser::new()
        .pure()
        .include(dir.path())
        .input(&contact_path)
        .parse_and_typecheck()?;

    println!(
        "  parse_and_typecheck returned {} file(s):",
        parsed.file_descriptors.len()
    );
    for fd in &parsed.file_descriptors {
        println!("    - {}", fd.name());
    }

    let mut fds = protobuf::descriptor::FileDescriptorSet::new();
    fds.file = parsed.file_descriptors;
    let bytes = fds.write_to_bytes()?;
    let pool = DescriptorPool::decode(bytes.as_slice())?;

    let contact_desc = pool
        .get_message_by_name("test.Contact")
        .ok_or_else(|| anyhow::anyhow!("test.Contact not found"))?;
    println!("  Found: {}", contact_desc.full_name());

    let address_desc = pool
        .get_message_by_name("test.Address")
        .ok_or_else(|| anyhow::anyhow!("test.Address not found (import dep missing from pool)"))?;
    println!("  Found: {}", address_desc.full_name());

    println!("  PASS");
    Ok(())
}

fn test_in_memory_api() {
    println!("=== Test 4: in-memory API probe ===");
    // protobuf_parse::Parser offers:
    //   .include(path)  — add include dir
    //   .input(path)    — add a .proto file by path (file must exist on disk)
    //   .parse_and_typecheck() → ParsedAndTypechecked
    //   .file_descriptor_set() → FileDescriptorSet  (strips dep files!)
    //
    // There is NO in-memory string input API (no .parse_str() or similar).
    // The pure Rust parser still requires files on disk for both the main
    // proto and any imported protos.
    //
    // Implication for the plugin: protobuf/schema must write the proto string
    // to a temp file before parsing. The tempfile crate handles this safely.
    //
    // Note: file_descriptor_set() is NOT suitable for import-using protos
    // because it filters out dependency files. The plugin must call
    // parse_and_typecheck() and build FileDescriptorSet from all file_descriptors.
    println!("  protobuf_parse::Parser: file-based only (no in-memory string input)");
    println!("  file_descriptor_set() strips dep files — use parse_and_typecheck() instead");
    println!("  Temp-file approach required for both main proto and imports");
    println!("  PASS");
}

fn main() -> anyhow::Result<()> {
    println!("--- elle-protobuf spike ---\n");

    test_basic_bridge()?;
    println!();

    test_roundtrip()?;
    println!();

    test_import_resolution()?;
    println!();

    test_in_memory_api();
    println!();

    println!("SPIKE OK");
    Ok(())
}
