#!/bin/sh

##
## FUNCTIONS DEFINITION
## scroll further down for script flow
##

function home_settings {
	# $1: Name of current file
	# $2: Name of recommended file
	# $3: Name of the setting
	OLDPWD="$PWD"
	cd ~
	OLDFILE="$1"
	NEWFILE="$2"
	NAME="$3"

	# download and make usual replacements
	(curl -s "https://raw.githubusercontent.com/EvolutionaryLinguisticsAssociation/Babel2/master/scripts/osx/$NEWFILE" |
	 sed -e "s|<USER>|`whoami`|g" |
	 sed -e "s|<CCL>|`which ccl`|g" > "$NEWFILE")
	ls "$OLDFILE"
	if [ $? -eq 0 ]; then
		echo
		echo "You already have a $NAME config file. What do you want to do?"
		KEEPASKING=1
		while [ $KEEPASKING -ne 0 ]; do
			echo
			echo "You can press L to check the settings we suggest with the less tool."
			echo "Otherwise, press W to overwrite the file or A to append to it."
			read -n1 -rsp "You can also press S to skip or Ctrl+C to abort: " KEY
			echo
			if [ "$KEY" = 'L' -o "$KEY" = 'l' ]; then
				less "$NEWFILE"
			else
				KEEPASKING=0
			fi
		done
		if [ "$KEY" = 'S' -o "$KEY" = 's' ]; then
			echo "Skipping step."
		elif [ "$KEY" = 'A' -o "$KEY" = 'a' ]; then
			cat "$NEWFILE" >> "$OLDFILE"
			echo "Appended config."
		elif [ "$KEY" = 'W' -o "$KEY" = 'w' ]; then
			mv "$NEWFILE" "$OLDFILE"
			echo "Overwritten config."
		fi
	else
		mv "$NEWFILE" "$OLDFILE"
		echo "Config placed."
	fi
	cd "$OLDPWD"
}


function brew_install {
	# $1: Command to be checked
	# $2: Name of package
	# $3: Options if any
	command -v $1
	if [ $? -eq 0 ]; then
		echo "$2 detected, skipping."
	else
		echo "$2 has not been found."
		echo "Installing..."
		brew install $2 $3
		if [ $? -eq 0 ]; then
			echo "Installed."
		else
			echo "Something wrong has happened, aborting. Contact the Babel team for help."
			exit 1
		fi
	fi
}

##
## END OF FUNCTIONS DEFINITION
##


##
## MAIN SCRIPT FLOW
##

echo
echo "Welcome to the Babel 2 installation assistant."
echo "This installer will set up your system for development with Babel 2 (including FCG and IRL)."
echo "Note: If anything goes wrong during the execution, it's safe to try to run it again. \
It will skip the steps already performed."
read -n1 -rsp "Press any key to continue or Ctrl+C to abort: "
echo
echo
echo


echo "== 1: OSX Development Tools =="
xcode-select -p
if [ $? -eq 0 ]; then
	echo "Development Tools detected, skipping."
else
	echo "Development Tools have not been found."
	echo "Invoking installer..."
	echo "A dialog should have been displayed."
	echo "Follow the instructions on the dialog to completion before performing any more steps."
	xcode-select --install
	read -n1 -rsp "If everything went OK, press any key to continue (or Ctrl+C to abort): "
	echo
fi
echo


echo "== 2: Homebrew install tool =="
command -v brew
if [ $? -eq 0 ]; then
	echo "Homebrew detected, skipping."
else
	echo "Homebrew has not been found."
	echo "Installing..."
	(ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" &&
	 brew doctor)
	if [ $? -eq 0 ]; then
		echo "Installed."
	else
		echo "Something wrong has happened, aborting. Contact the Babel team for help."
		exit 1
	fi
fi
echo

echo "== 3: CCL Lisp compiler =="
## brew_install "ccl" "clozure-cl"
command -v ccl
if [ $? -eq 0 ]; then
	echo "CCL detected, skipping."
else
	echo "Downloading and installing CCL"
	curl -OL "https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-darwinx86.tar.gz"
	tar -xvzf ccl-1.11.5-darwinx86.tar.gz
	mv ./ccl/scripts/ccl /usr/local/bin/
	mv ./ccl/scripts/ccl64 /usr/local/bin/
	if [ ! -d "/usr/local/src/ccl" ]; then
		sudo mkdir -p /usr/local/src/ccl
	fi
	sudo mv -v ./ccl/* /usr/local/src/ccl/
fi
echo


echo "== 4: Gnuplot =="
if [ -d "/Applications/Aquaterm.app" ]; then
	echo "AquaTerm detected, skipping"
else
	brew cask install aquaterm
fi
brew_install "gnuplot" "gnuplot" "--with-aquaterm --with-cairo"
echo


echo "== 5: Graphviz =="
brew_install "dot" "graphviz"
echo

echo "== 6: Editors =="
echo "There are several editors you can use for Babel development."
echo "We recommend the installation of Emacs. This is the default."
echo
echo "You can also use the professional editor Lispworks for development."
echo "However, the free version of Lispworks won't work with Babel due to memory limits."
echo "We can't install Lispworks for you, but we can set up the config for Babel."
echo
read -n1 -rsp "Press S to skip, L for Lispworks, Ctrl+C to abort, or any other key for Emacs: " KEY
echo
if [ "$KEY" = 'S' -o "$KEY" = 's' ]; then
	echo "Skipping editor."
	echo
elif [ "$KEY" = 'L' -o "$KEY" = 'l' ]; then
	echo
	echo "== 6: Lispworks settings =="
	home_settings ".lispworks" "lispworks-babel" "Lispworks"
	echo
else
	echo
	echo "== 6: Emacs editor =="
	if [ -d "/Applications/Emacs.app" ]; then
		echo "Emacs detected, skipping"
	else
		brew tap railwaycat/emacsmacport
		brew_install emacs emacs-mac
		ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications
	fi
	echo


	echo "== 6a: Slime plugin =="
	# see if there is a path to load slime from somewhere
	(cat ~/.emacs | sed -Ene "s/\(add-to-list \'load-path \"(.*)\"\).*$/\1/p" | grep slime ||
	# check also default loading folder ~/.emacs.d/
	find ~/.emacs.d -type f -name '*slime*')
	if [ $? -eq 0 ]; then
		echo "Slime detected, skipping."
	else
		echo "Slime has not been found."
		echo "Installing..."
		(cd ~ && git clone https://github.com/slime/slime.git)
		if [ $? -eq 0 ]; then
			echo "Installed."
		else
			echo "Something wrong has happened, aborting. Contact the Babel team for help."
			exit 1
		fi
	fi
	echo


	echo "== 6b: Emacs/Slime settings =="
	home_settings ".emacs" "emacs-babel" "Emacs"
	echo
fi

echo "== 7: Babel2 framework =="
ls ~/Babel2
if [ $? -eq 0 ]; then
	echo "Babel2 detected, skipping."
else
	echo "Babel2 has not been found."
	echo "Cloning the latest version from Git..."
	brew_install "git" "git"
	(git clone https://github.com/EvolutionaryLinguisticsAssociation/Babel2.git ~/Babel2)
	echo "Installed"
fi
echo


echo "== 8: Babel2 config for CCL =="
home_settings ".ccl-init.lisp" "ccl-init-babel.lisp" "CCL init"
echo

echo "== 9: Quicklisp =="
ls ~/quicklisp
if [ $? -eq 0 ]; then
	echo "Quicklisp detected, skipping."
else
	echo "Quicklisp has not been found"
	echo "Installing..."
	curl -O "https://beta.quicklisp.org/quicklisp.lisp"
	echo "The script will now open a CCL session to install Quicklisp"
	echo "If CCL does not exit automatically, enter (quit)"
	read -n1 -rsp "Press any key to continue or Ctrl+C to abort: "
	ccl -l quicklisp.lisp -e '(quicklisp-quickstart:install)' -e '(ql:quickload :cl-ppcre)' -e '(ql:quickload :cl-who)' -e '(quit)' -b
	rm quicklisp.lisp
	echo "Installed."
fi

echo "== 10: Testing Babel2 =="
# We omit the browser test because it entails keeping ccl running and it complicates matters
# it usually never fails anyway (compared to possible graphviz/gnuplot issues)
echo "The test consists in opening a PDF file with a diagram and displaying a window with a sinus function."
read -n1 -rsp "Press S to skip, Ctrl+C to abort, or any other key to run the test: " KEY
echo
if [ "$KEY" = 'S' -o "$KEY" = 's' ]; then
	echo "Skipping test."
else
	echo "Give the system some time to run the tests. It shouldn't take more than ten seconds..."
	ccl -l "~/Babel2/test-babel-installation.lisp" --eval '(quit)' -b
	echo
	echo "If the file didn't open or the window didn't display,"
	echo "there is a problem with your installation. Contact the Babel team for help."
	echo
	read -n1 -rsp "Press any key to continue or Ctrl+C to abort: "
	echo
	echo
fi
echo

echo "-- 11: Warning! =="
echo "On some versions of mac OS, Apple has decided to disable local web servers by default."
echo "In order to use the Babel2 web interface, this needs to be enabled again."
echo "This can be done by following these instructions:"
echo "---------------------------------------------"
echo "https://discussions.apple.com/docs/DOC-12034"
echo "---------------------------------------------"
echo "After following these instructions, restart your machine to make sure the changes took place."
echo


echo
echo "Setup has finished. Happy hacking!"
echo
