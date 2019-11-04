# Dotfiles

## Installation

**Warning:**
Even tho these dotfiles are installed in a dedicated directory and are easy to uninstall if you dont like them, you should first fork this repository, review the code, and remove things you don’t want or need. Don’t blindly use my settings unless you know what that entails. Use at your own risk!

```bash
sh -c "$(curl -fsSL https://raw.githubusercontent.com/schulke-214/dotfiles/master/install)"
```

If you want to install these dotfiles into another dir than `~/dotfiles` you should provide the target directory in the `DOTFILES` variable like this:

```bash
sh -c "DOTFILES=~/your-custom-dotfiles-folder $(curl -fsSL https://raw.githubusercontent.com/schulke-214/dotfiles/master/install)"
```

### Updates

To update you local dotfiles installation simply run `$DOTFILES/update`.
This will compare your local environment with the remote, clone changes, update dependencies (eg. nvm, oh-my-zsh etc.) and clear unused files.

### Uninstalling

To remove these dotfiles all you need to do is to run a `rm -rf $DOTFILES`, open your `.zshrc` and remove the `source <your-dotfiles-path>/zshrc` statement. If you refer in any other config files to `$DOTFILES` you have to remove these references aswell.

## Usage

### Environment Variables

| Identifier | Description                                              | Default      |
| :--------- | :------------------------------------------------------- | :----------- |
| `DOTFILES` | This is the directory where this repo gets installed in. | `~/dotfiles` |
