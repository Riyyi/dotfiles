#!/bin/sh

BACKUP_COUNT_MAX=5

# Backup individual files
FILES="
/etc/netctl/eth0-dhcp
"

# Backup all files from folder
FOLDER_FILES="
/etc/netctl
"

# Backup complete folder (recursively)
FOLDERS="
/etc/netctl
"

# Database credentials
USER="username"
PASSWORD="password"
HOST="127.0.0.1"

# Database names
DATABASES="
example
example2
"

# ------------------------------------ #

### Remove / create backup directories ###

if [ "$(dirname "$0")" != "." ]; then
	echo "Please run this script from the directory it resides."
	exit 1
fi

# All directories
BACKUP_FILES="$(find . -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort -r)"

# Current amount of backups
BACKUP_COUNT=$(echo "$BACKUP_FILES" | wc -l)

# Remove the oldest backup folders
if [ "$BACKUP_COUNT" -ge $BACKUP_COUNT_MAX ]; then
	BACKUPS_OLDEST="$(echo "$BACKUP_FILES" | sed -nE "
		1,${BACKUP_COUNT_MAX}d;
		s/^([0-9]{2}|-|_|:){12}$/\0/p
	")"

	printf "%s\n%s\n" "Removing directories:" "$BACKUPS_OLDEST"
	rm -rf $BACKUPS_OLDEST
fi

# Execution location
DIR="$(pwd)"

# Create new backup folder
BACKUP_NAME="$(date +%Y-%m-%d_%H:%M:%S)"
mkdir "$DIR/$BACKUP_NAME"

### Start backup procedure ###

# Individual files
for FILE in $FILES; do
	mkdir -p "$DIR/$BACKUP_NAME/$(dirname "$FILE")"
	cp "$FILE" "$DIR/$BACKUP_NAME/$FILE"
done

# All files inside folder
for FOLDER_FILE in $FOLDER_FILES; do
	mkdir -p "$DIR/$BACKUP_NAME/$FOLDER_FILE"

	# Usind find -> read ensures filename space support
	find "$FOLDER_FILE" -maxdepth 1 -type f | while read -r FILE; do
		cp "$FILE" "$DIR/$BACKUP_NAME/$FILE"
	done
done

# Complete folder
for FOLDER in $FOLDERS; do
	mkdir -p "$DIR/$BACKUP_NAME/$FOLDER"
	cp -r "$FOLDER" "$DIR/$BACKUP_NAME/$(dirname "$FOLDER")"
done

# Set default file permissions
umask 177
# MYSQL Dump
for DATABASE in $DATABASES; do
	# mysqldump --user=$USER --protocol=socket -S /var/run/mysqld/mysqld.sock "$DATABASE"
	mysqldump --user=$USER --password=$PASSWORD --host=$HOST "$DATABASE" \
			  > "$DIR/$BACKUP_NAME/$DATABASE-$BACKUP_NAME.sql"
done
