# This relies on the python-keyring module. This method works
# out-of-the-box on OSX, and can be made to work for gnome-keyring or
# kwallet by installing the corresponding adapter. More secure storage
# options described at:
# https://github.com/sup-heliotrope/sup/wiki/Securely-Store-Password

import keyring
