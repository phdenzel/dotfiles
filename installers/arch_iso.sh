#!/bin/bash


### Download latest archlinux iso
arch_mirror="pkg.adfinis.com"
# Other mirrors just in case
# "pkg.adfinis.com"
# "mirror.puzzle.ch"
# "theswissbay.ch"
# "mirror.rackspace.com"
latest_dir="archlinux/iso/latest"

wget -c "${arch_mirror}/${latest_dir}/md5sums.txt"
sed -i '/archlinux-bootstrap/d' md5sums.txt
read md5sum arch_iso <<< $(cat md5sums.txt)
wget -c "${arch_mirror}/${latest_dir}/${arch_iso}"
md5sum -c md5sums.txt
