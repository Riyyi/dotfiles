# Dotfiles

This is a collection of dotfiles and scripts for my i3 setup, previewed below:

![Screenshot](screenshot.png)

### Software:

All packages that are manually installed via pacman -S, can be found in the packages file,
dependencies are not included.


### Configuration:

Below are all the system changes that were made.<br>
$ = command<br>
<> = variable<br>
() = action

Boot defaults:
```
$ vim /boot/loader/loader.conf
timeout 3
default arch
```
Boot config:
```
<uuid> = $ blkid # from the / 'root' partition
$ vim /boot/loader/entries/arch.conf
title Arch Linux
linux /vmlinuz-linux
initrd /intel-ucode.img
initrd /initramfs-linux.img
options root=PARTUUID=<uuid> rw
```
Predictable network interface names:
```
$ ln -s /dev/null /etc/systemd/network/99-default.link
```
Network:
```sh
$ systemctl enable netctl-auto@wlan0.service
```
Give wheel users sudo permission:
```sh
$ nano /etc/sudoers
(add)Defaults insults
(uncomment)%wheel ALL=(ALL) ALL
```
Add user:
```sh
$ useradd -m -G wheel -s /bin/bash <username>
$ passwd <username>
```
Installing pacaur:
```sh
$ git clone https://aur.archlinux.org/trizen.git
$ cd trizen
$ makepkg -si
$ cd ..

$ trizen -S trizen
$ rm -rf trizen
```
Switch shell to zsh:
```sh
$ chsh -s /bin/zsh
```
Pacman colors:
```sh
$ nano /etc/pacman.conf
(uncomment)Color
```
Git:
```sh
$ git config --global user.email "<email address>"
$ git config --global user.name "<name>"
```
Tlp:
```sh
$ systemctl enable tlp.service
$ systemctl enable tlp-sleep.service

$ nano /etc/default/tlp
(edit)SOUND_POWER_SAVE_ON_BAT=0
```
Trim:
```sh
$ systemctl enable fstrim.timer
```
Clock internet sync:
```sh
$ timedatectl set-ntp true
```
Mail gpg:
```sh
$ gpg --decrypt <backup.pgp> | gpg --import
$ gpg --search-keys <email address>
```
Give permission to /dev/ttyUSB0 for PlatformIO:
```sh
$ gpasswd -a <user> uucp
```
Set Firefox as default browser:
```sh
$ xdg-settings set default-web-browser firefox.desktop
```
Prevent bricking motherboard by mounting efivars read-only:<br>
https://wiki.archlinux.org/index.php/Unified_Extensible_Firmware_Interface#Mount_efivarfs
```sh
$ nano /etc/fstab
(add)efivarfs    /sys/firmware/efi/efivars    efivarfs    ro,nosuid,nodev,noexec,noatime 0 0
```
