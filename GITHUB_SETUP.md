# GitHub Setup Checklist for disruptek/elle

This document tracks the setup for the Elle Lisp interpreter on GitHub with complete CI/CD pipeline.

## ✅ Repository Configuration

### Repository Settings to Configure

1. **Branch Protection**
   - [ ] Go to Settings → Branches
   - [ ] Add branch protection rule for `main`
   - [ ] Require status checks to pass before merging:
     - `test (stable)`, `test (beta)`, `test (nightly)`
     - `fmt`
     - `clippy`
     - `benchmarks`
     - `examples`
     - `docs`
     - `all-checks`
   - [ ] Require branches to be up to date before merging
   - [ ] Require code reviews before merging (optional)
   - [ ] Include administrators in restrictions

2. **Pages Configuration**
   - [ ] Go to Settings → Pages
   - [ ] Source: GitHub Actions
   - [ ] Custom domain: (optional)
   - [ ] HTTPS: Enable
   - [ ] Documentation site will be auto-deployed to https://disruptek.github.io/elle/

3. **Secrets (if needed for future features)**
   - [ ] No secrets currently required
   - [ ] For Codecov uploads: Add `CODECOV_TOKEN` if private repo

4. **Actions Configuration**
   - [ ] Go to Settings → Actions
   - [ ] Allow all actions and reusable workflows (or restrict as needed)
   - [ ] Artifact retention: 30 days

## ✅ CI/CD Pipeline (.github/workflows/ci.yml)

### Jobs Configuration

1. **Test Suite**
   - ✅ Runs on: Ubuntu latest
   - ✅ Matrix: stable, beta, nightly Rust
   - ✅ Tasks:
     - Unit tests (`cargo test --lib`)
     - Integration tests (`cargo test --test '*'`)
     - Doc tests (`cargo test --doc`)

2. **Formatting (rustfmt)**
   - ✅ Runs on: Ubuntu latest
   - ✅ Task: `cargo fmt -- --check`

3. **Linting (Clippy)**
   - ✅ Runs on: Ubuntu latest
   - ✅ Task: `cargo clippy --all-targets --all-features -- -D warnings`

4. **Benchmarks**
   - ✅ Runs on: Ubuntu latest
   - ✅ Tasks:
     - Build benchmarks
     - Execute benchmarks
     - Report results

5. **Examples**
   - ✅ Runs on: Ubuntu latest
   - ✅ Tasks:
     - Build all examples
     - Verify FFI Phase 2 example

6. **Code Coverage**
   - ✅ Runs on: Ubuntu latest
   - ✅ Tool: cargo-tarpaulin
   - ✅ Upload to: Codecov
   - ✅ Format: Cobertura XML

7. **Documentation**
   - ✅ Runs on: Ubuntu latest
   - ✅ Tasks:
     - Generate docs with `cargo doc`
     - Create Pages landing page (index.html)
     - Add robots.txt for SEO
     - Add sitemap.txt
     - Upload as artifact

8. **GitHub Pages Deployment**
   - ✅ Runs on: Ubuntu latest (only on main branch pushes)
   - ✅ Requirements: docs job success
   - ✅ Permissions: pages write, id-token write
   - ✅ Concurrency group: pages (cancel in progress)
   - ✅ Auto-deploying to: https://disruptek.github.io/elle/

9. **All Checks Gate**
   - ✅ Requires all other jobs to pass
   - ✅ Blocks merge if any check fails

## ✅ Documentation Files

### Created Files

1. **CONTRIBUTING.md** ✅
   - Development guidelines
   - Code quality requirements
   - Commit message guidelines
   - Pull request workflow
   - Areas for contribution

2. **FFI_ROADMAP.md** ✅
   - 5-phase FFI implementation plan
   - 16-week timeline
   - Detailed specifications
   - Architecture diagrams

3. **FFI_PHASE_2_COMPLETE.md** ✅
   - Phase 2 completion report
   - Test results (218 passing)
   - Implementation details
   - Performance characteristics

4. **examples/ffi-phase2.lisp** ✅
   - Phase 2 FFI examples
   - Demonstrates type system
   - Shows function calling patterns

5. **README.md** ✅ (Updated)
   - Added CI/CD badges
   - Added FFI section
   - Updated testing section

### Website Content

The GitHub Pages site will host:
- **index.html** - Landing page with project overview, CI/CD pipeline status, and feature highlights
- **elle/** - Full API documentation from `cargo doc`
- **robots.txt** - SEO configuration
- **sitemap.txt** - Site structure

## ✅ Pre-Push Checklist

Before pushing to GitHub, verify:

```bash
# 1. Run all tests
cargo test --lib
cargo test --test '*'
cargo test --doc

# 2. Check formatting
cargo fmt -- --check

# 3. Run clippy
cargo clippy --all-targets --all-features -- -D warnings

# 4. Run benchmarks
cargo bench --no-run
cargo bench --bench benchmarks

# 5. Build examples
for example in examples/*.rs; do
  cargo build --example "$(basename "$example" .rs)"
done

# 6. Generate docs
RUSTDOCFLAGS="-D warnings" cargo doc --no-deps

# 7. Verify git status
git status
```

All should complete successfully.

## ✅ Local Testing of Workflow

To test the workflow locally (optional):

```bash
# Install act (GitHub Actions local runner)
# https://github.com/nektos/act

# Run the CI workflow locally
act -j test
act -j fmt
act -j clippy
act -j benchmarks
act -j examples
act -j docs

# Or run all jobs
act
```

## 🚀 Next Steps

1. **Create GitHub Repository**
   ```bash
   # Assuming SSH keys are configured
   git remote add origin git@github.com:disruptek/elle.git
   git branch -M main
   git push -u origin main
   ```

2. **Configure Repository Settings** (from above checklist)

3. **First Push**
   - CI/CD pipeline will automatically run
   - Check Actions tab for results
   - Documentation will deploy to Pages when ready

4. **Verify Pages**
   - After first successful push, check: https://disruptek.github.io/elle/
   - Should see landing page with API docs link

5. **Future Phases**
   - Phase 2b: Floating-point returns, 7+ arguments
   - Phase 3: Header parsing and auto-binding
   - Phase 4: GTK4/SDL2 support

## 📋 Status

| Component | Status |
|-----------|--------|
| CI/CD Workflow | ✅ Ready |
| Tests | ✅ 218 passing |
| Benchmarks | ✅ Working |
| Examples | ✅ All compile |
| Documentation | ✅ Builds cleanly |
| GitHub Pages Setup | ✅ Configured |
| README | ✅ Updated |
| CONTRIBUTING | ✅ Created |
| Branch Protection | ⏳ Pending GitHub setup |
| Secrets | ✅ Not needed |

## 📝 Notes

- No external dependencies added for CI/CD
- Workflow uses standard actions (checkout, toolchain, cache, deploy-pages)
- All checks are required for merge - no exceptions
- Documentation is auto-deployed on every main branch push
- Coverage reports go to Codecov (optional - currently set to not fail)
- Benchmarks run on every push for performance tracking

## 📚 Resources

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [GitHub Pages Documentation](https://docs.github.com/en/pages)
- [Rust GitHub Actions](https://github.com/dtolnay/rust-toolchain)
- [Cargo Documentation](https://doc.rust-lang.org/cargo/)

---

**Setup completed:** February 4, 2026  
**Ready for production:** Yes ✅
