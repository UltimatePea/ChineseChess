# chinesechess

This program implements game Xiangqi, with the minimax algorithm. 
Good for use with 2 players, 1 player and 1 AI, or two AIs.

The game uses VI key bindings that should be familiar to most programmers, i.e. 
`hjkl` to move left, down, up right,
`u` and `ctrl-R` to undo and redo,
`m` to move, `y` to confirm and `n` to cancel.

Instructions are also given in the game.

The game has a command system that's similar to VI's.
You can use the system to enable/disable AI and to control the game play.
Type `:help` to see list of commands.


Note that the game hides the terminal cursor. Please use `ctrl-D` or `:quit`
to quit the game instead of `ctrl-C`.


## Demo

![demo](.README/demo.gif)

## Install

### On macOS

1. Install Brew 

2. Install Haskell Platform

3. Install Stack

4. Run `stack install`

5. Run `chinesechess-exe`.

```

# Get Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install Haskell and Stack
brew cask install haskell-platform
brew install curl
curl -sSL https://get.haskellstack.org/ | sh
stack setup

# Download
git clone https://github.com/UltimatePea/ChineseChess.git
cd ChineseChess

# Install this software
stack install
chinesechess-exe
```


