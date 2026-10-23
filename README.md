# dot-files

Settings for the tools I use: shell, git, editors, terminal, and the tool versions mise installs. This repo is declarative. It says *what* each tool's configuration is, not *how* a machine gets set up; installing packages and deciding which of these land on which machine lives in a separate, private provisioning repo.

## Layout

Each top-level directory except `vscode-tools/` is a **Stow package**: its internal structure mirrors where its contents land under `$HOME`. For example, `zsh/.zshrc` → `~/.zshrc`; `mise/.config/mise/config.toml` → `~/.config/mise/config.toml`.

```
bash/        .bashrc, .bash_aliases, .bash_prompt, .profile, .inputrc
zsh/         .zshrc
git/         .gitconfig (includes ~/.gitconfig.local for per-machine identity — see below)
tmux/        .tmux.conf
emacs/       .emacs.d/
vim/         .vim/ and .config/nvim/ — vanilla baseline, not actively used for dev
mise/        .config/mise/config.toml — base tool versions every machine gets
mise-extras/ .config/mise/conf.d/extras.toml — extra tools, layered on where wanted
ghostty/     .config/ghostty/config
starship/    .config/starship.toml
sqlite/      .sqliterc
yt-dlp/      .config/yt-dlp/config
vscode/      settings.json, keybindings.json (target dir differs by OS — see below)

vscode-tools/   VS Code extension install script — not a Stow package
```

## Using it

```shell
git clone https://github.com/bgturner/dot-files.git ~/.dot-files
cd ~/.dot-files
stow -t "$HOME" zsh bash git tmux emacs vim mise starship sqlite   # plus any extras: mise-extras ghostty yt-dlp
stow -t "$HOME/Library/Application Support/Code/User" vscode        # macOS
stow -t "$HOME/.config/Code/User" vscode                            # Linux
mise install
```

## mise: base and extras

`mise/` holds the tools every machine gets. `mise-extras/` adds `~/.config/mise/conf.d/extras.toml`, and mise merges every `conf.d/*.toml` into the global config, so a machine without `mise-extras` stowed never installs those tools. Pin real versions (`mise latest <tool>`), not guesses.

The rule of thumb for what belongs in mise: if it's a versioned CLI tool, mise owns it, the same version everywhere. Anything needing real OS integration (a login shell, a background service, a system library, fonts) belongs to the OS package manager instead.

## Per-machine secrets and overrides

Nothing machine-specific or sensitive is tracked here. Git-ignored local files carry that instead, each sourced/included by its tracked counterpart if present:

- `~/.zshrc.local` / `~/.bashrc.local` — env var overrides (e.g. `CLAUDE_CODE_USE_VERTEX`, `CLOUD_ML_REGION` on the work Mac)
- `~/.gitconfig.local` — per-machine git identity (`[user] name`/`email`)

## Adding a new dotfile

Either add it to an existing Stow package if it belongs to that tool, or create a new top-level directory named after the package with the file at the path it should have under `$HOME` (e.g. a new `foo/.config/foo/config`).
