# dot-files

Personal dotfiles and provisioning for four machines: a personal Intel Mac, personal Ubuntu, personal Debian, and a work Mac. One consolidated pipeline instead of divergent per-machine scripts.

## Architecture

Three layers, each owning one job:

| Layer | Owns | Scope |
|---|---|---|
| **[mise](https://mise.jdx.dev)** | Language runtimes and CLI tool versions (`node`, `python`, `rust`, `gh`, `ripgrep`, `jq`, `tmux`, ...) — see `mise/.config/mise/config.toml` | All machines, including the work Mac |
| **[Ansible](https://docs.ansible.com)** | OS packages and system setup (`provision/personal/`) | Personal machines only — **not** the work Mac |
| **[GNU Stow](https://www.gnu.org/software/stow/)** | Symlinking dotfiles from this repo into `$HOME` | All machines |

The rule of thumb: if it's a versioned CLI tool, mise owns it, the same version everywhere. If it needs real OS integration (a login shell, a background service, a system library other packages link against, fonts), it stays on the OS package manager. Nothing installs the same thing two ways — see each `provision/personal/roles/*/tasks/main.yml` for the comment explaining what's deliberately *not* there and why.

The work Mac is enterprise-locked-down, so it's provisioned by a plain script (`bootstrap-mac.sh`) instead of Ansible — same mise + Stow layers, just no Ansible dependency.

## Machines

| Machine | Provisioner | Entry point |
|---|---|---|
| Personal Mac | mise + Ansible (`mac` role) + Stow | `bootstrap.sh` |
| Personal Ubuntu | mise + Ansible (`linux` role) + Stow | `bootstrap.sh` |
| Personal Debian | mise + Ansible (`linux` role) + Stow | `bootstrap.sh` |
| Work Mac | mise + Stow (no Ansible) | `bootstrap-mac.sh` |

## Bootstrapping a personal machine

```shell
git clone git@github.com:bgturner/dot-files.git ~/.dot-files
cd ~/.dot-files
./bootstrap.sh
```

`bootstrap.sh` is a thin orchestrator — it doesn't install anything itself. It runs, in order:

1. `ansible-playbook` against `provision/personal/` (limited to the current host via `hostname -s` — see `provision/personal/inventory` and `provision/personal/readme.org` for adding a new machine)
2. `stow` to symlink every package below into `$HOME`
3. `mise install` to sync every pinned tool version

## Bootstrapping the work Mac

```shell
git clone git@github.com:bgturner/dot-files.git ~/.dot-files
cd ~/.dot-files
./bootstrap-mac.sh
```

Installs Homebrew and mise directly (no Ansible), then runs the same Stow + `mise install` steps.

## Layout

Each top-level directory except `provision/` and `vscode-tools/` is a **Stow package** — its internal structure mirrors where its contents land under `$HOME`. For example, `zsh/.zshrc` → `~/.zshrc`; `mise/.config/mise/config.toml` → `~/.config/mise/config.toml`.

```
bash/       .bashrc, .bash_aliases, .bash_prompt, .profile, .inputrc
zsh/        .zshrc
git/        .gitconfig (includes ~/.gitconfig.local for per-machine identity — see below)
tmux/       .tmux.conf
emacs/      .emacs.d/
vim/        .vim/ and .config/nvim/ — vanilla baseline, not actively used for dev
mise/       .config/mise/config.toml — every mise-managed tool version
ghostty/    .config/ghostty/config
starship/   .config/starship.toml
sqlite/     .sqliterc
vscode/     settings.json, keybindings.json (target dir differs by OS — see bootstrap.sh)

vscode-tools/   VS Code extension install script — not a Stow package
provision/      Ansible playbook for personal machines (see provision/personal/readme.org)
bootstrap.sh        Orchestrator for personal machines
bootstrap-mac.sh    Standalone provisioner for the work Mac
gconf-settings.sh   Standalone GNOME terminal appearance tweaks (Linux desktop only, not part of the pipeline above)
```

## Per-machine secrets and overrides

Nothing machine-specific or sensitive is tracked here. Three git-ignored local files carry that instead, each sourced/included by its tracked counterpart if present:

- `~/.zshrc.local` / `~/.bashrc.local` — env var overrides (e.g. `CLAUDE_CODE_USE_VERTEX`, `CLOUD_ML_REGION` on the work Mac)
- `~/.gitconfig.local` — per-machine git identity (`[user] name`/`email`)

## Adding a new tool

Ask first: is it a CLI tool with a version that matters? Add it to `mise/.config/mise/config.toml`, pinned to a real version (`mise latest <tool>`, don't guess). Otherwise it belongs in Ansible (`provision/personal/roles/mac|linux/tasks/main.yml`) for personal machines and/or `bootstrap-mac.sh` for the work Mac.

## Adding a new dotfile

Either add it to an existing Stow package if it belongs to that tool, or create a new top-level directory named after the package with the file at the path it should have under `$HOME` (e.g. a new `foo/.config/foo/config`), then add the package name to the `stow -d . -t "$HOME" ...` line in `bootstrap.sh`.
