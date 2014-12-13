#! /bin/sh

ensure_copies () {
	source="$1"
	prefix="$2"
	target="$prefix/$source"
	mkdir -p "$target"/{.trash,.to-delete}
	find "$target/.to-delete/" -mindepth 1 -execdir rm -r '{}' ';'

	wanted="$(cat | sort)"

	existing="$(cd "$target"; ls | sort)"

	for i in $existing; do touch "$target/.to-delete/$i"; done
	for i in $wanted; do 
		[ -e "$target/$i" ] || 
		{
			echo "Copying $i .." >&2
			cp -r "$source/$i" "$target/$i"
			echo "Done" >&2
		}; 
		rm -f "$target/.to-delete/$i"
	done
	for i in $(cd "$target/.to-delete"; ls); do 
		echo "Removing $i" >&2
		chmod u+w -R "$target/$i"
		mv "$target/$i" "$target/.trash/$i"
	done
	echo "Removing garbage" >&2
	rm -rf "$target/.trash"
	echo "Done" >&2
}

ensure_nix_copies () {
	source="${NIX_STORE_DIR:-/nix/store}"
	prefix="$1"
	xargs nix-store -qR | xargs -l1 basename |
	ensure_copies "$source" "$prefix"
}

"$@"
