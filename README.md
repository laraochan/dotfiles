# :jellyfish:laraoのdotfile(macosバージョン):jellyfish:

## setup

### 最短のセットアップ

```bash
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply laraochan
```

### 通常のセットアップ

1. chezmoiのインストール
```bash
# MacOSの場合はHomebrewを使ってインストールする
brew install chezmoi
```

2. リポジトリを初期化
```bash
chezmoi init https://github.com/laraochan/dotfiles.git
```

3. 取得した設定を適用
```bash
chezmoi apply
```

## setting update

```bash
chezmoi cd
chezmoi git pull
chezmoi apply
```
