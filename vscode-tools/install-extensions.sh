#!/bin/bash
# Ensure that `code` is in your path and then run this script
while IFS="" read EXT
	do code --install-extension $EXT;
done < installed-extensions
