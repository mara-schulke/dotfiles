# Dotfiles

## Installation

**Warning:** If you want to give these dotfiles a try, you should first fork this repository, review the code, and remove things you don’t want or need. Don’t blindly use my settings unless you know what that entails. Use at your own risk!

```bash
sh -c "$(curl -fsSL dotfiles.maximilianschulke.com)"
```

### Updates

To update you local dotfiles installation simply run `$DOTFILES/update`.
This will compare your local environment with the remote, clone changes, update dependencies (eg. nvm, oh-my-zsh etc.) and clear unused files.

### Uninstalling

Simply run `$DOTFILES/uninstall` - this simply runs `rm -rf $DOTFILES` and removes references setup by the `install` script.

#### Uninstalling Manually

To remove these dotfiles manually all you need to do is to run a `rm -rf $DOTFILES`, open your `.zshrc` and remove the `source <your-dotfiles-path>/zshrc` statement.

## Usage

### Environment Variables

| Identifier | Description                                              | Default      |
| :--------- | :------------------------------------------------------- | :----------- |
| `DOTFILES` | This is the directory where this repo gets installed in. | `~/dotfiles` |

### Functions

tbd
