#!/bin/bash

### Download latest archlinux iso
arch_mirror="pkg.adfinis.com"
# Other mirrors just in case
# "pkg.adfinis.com"
# "mirror.puzzle.ch"
# "theswissbay.ch"
# "mirror.rackspace.com"
latest_dir="archlinux/iso/latest"

if [ $# -eq 0 ]; then
    wget -c "${arch_mirror}/${latest_dir}/md5sums.txt"
    sed -i '/archlinux-bootstrap/d' md5sums.txt
    read md5sum arch_iso <<< $(cat md5sums.txt)
    wget -c "${arch_mirror}/${latest_dir}/${arch_iso}"
    md5sum -c md5sums.txt
else
    arch_iso = "$1"
fi;

### Start installation
qemu-img create -f qcow2 /var/lib/libvirt/images/arch.qcow2 64G
virt-install \
    -n arch \
    --memory 8192 \
    --vcpus 4 \
    --disk path=/var/lib/libvirt/images/arch.qcow2 \
    --video model=virtio \
    --os-variant archlinux \
    --boot loader=/usr/share/OVMF/OVMF_CODE.fd \
#    --network bridge=br10 \
    --cdrom $arch_iso


### Look at all vms and imgs
# virsh list --all
# virsh vol-list --pool vg0
### Delete vm
# virsh destroy arch
# virsh undefine --nvram arch
# virsh vol-delete --pool default --vol arch.qcow2
