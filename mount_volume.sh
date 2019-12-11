#!/bin/bash

set -euo pipefail

#################################
# Script to mount an EBS volume #
# CAUTION: Extremely dangerous! #
#################################

default_file_system="xfs"

#################################
# Detect volumes with no partition and no mountpoint
#################################
disks=($(lsblk | grep "disk[[:space:]]*$" | grep -o "^[[:alnum:]]\+"))
for disk in ${disks[@]}
do
    let n_partitions=$(lsblk | grep ${disk} | grep -c "part") || true
    if [ ${n_partitions} -eq 0 ]
    then
	unpartitioned_disks+=(${disk})
    fi
done

if [ -z ${unpartitioned_disks+x} ]
then
    echo "No unpartitioned disks without a mountpoint."
    echo "Nothing to do."
    exit 0
fi

#################################
# Iterate through disks with no filesystem nor mountpoint
#################################
let i=1
for disk in ${unpartitioned_disks[@]}
do
    # Detect whether there is a pre-existing filesystem on the volume
    file_system=$(sudo file -s /dev/${disk})

    if [ ".${file_system}" != "./dev/${disk}: data" ]
    then
	>&2 echo "File system present. Aborting!"
	>&2 echo ${file_system}
	exit 1
    else
	#################################
	# Create new filesystem
	#################################
	sudo mkfs -t ${default_file_system} /dev/${disk} >/dev/null

	#################################
	# Create new mountpoint
	#################################
	# Skip over any previously used mount points
	while [ -d /data${i} ]
	do
	    let i+=1
	    if [ $i -ge 100 ]
	    then
		>&2 echo "Unable to find available mount point in 100 tries. Aborting!"
		exit 1
	    fi
	done
	sudo mkdir /data${i}
	sudo mount /dev/${disk} /data${i}
	sudo chown -R ${USER}: /data${i}

	#################################
	# Automatically mount attached volume after reboot
	#################################
	sudo cp -p /etc/fstab /etc/fstab.orig
	uuid_regex="[[:alnum:]]\{8\}-[[:alnum:]]\{4\}-[[:alnum:]]\{4\}-[[:alnum:]]\{4\}-[[:alnum:]]\{12\}"
	uuid=$(sudo lsblk -o +UUID | grep ${disk} | grep -o "${uuid_regex}$")
	sudo -- bash -c "echo 'UUID=${uuid}  /data${i}  ${default_file_system}  defaults,nofail  0  2' >> /etc/fstab"

	echo "Successfully mounted /dev/${disk} to /data${i} for user ${USER}"
    fi

    if [ $i -ne ${#unpartitioned_disks[@]} ]
    then
	echo ""
    fi
    let i+=1
done

exit 0
