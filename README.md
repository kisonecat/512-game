# The 512 game -- just like 2048, but its fits in a boot sector.

![Screenshot of the game in action](screenshot.png?raw=true "A screenshot of the game in action")

Depends on `nasm` to build.

After running `make`, you will have `fivetwve.com` and `fivetwve.img`.

You can run `fivetwve.com` via `dosbox` with `dosbox fivetwve.com`.

The contents of `fivetwve.img` are suitable for a boot sector.  You can boot this by running `qemu-system-i386 -fda fivetwve.img`.

