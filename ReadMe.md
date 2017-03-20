Organizing dotfiles
-------------------

So actually I decided to start publishing my dotfiles. This is not due
to the fact, that I think my configuration is in any way superior. It
is much more that I'm able to have some kind of backup of my own
configuration. Another nice side effect is, that I'm able to go back
to a working version of a configuration. And also I'm much faster able
to get a new system working, the way I'm used to. 

So a lot of good reasons to publish dotfiles :-). 

In the following I would like to show you three things:

  * Folders - where do i put my files
  * Git - how do i manage my git repository
  * Scripts - how to add something

Folders
-------

As I'm going to show you in the next section, I will put my dotfiles
under version control. So typically dotfiles live within home
directories. But as everything will be within my home directory, I do
not like to just put my home under version control. I therefore need
another location to store my dotfiles. 

For various other reasons I have a folder within my home, called
`localStorage`. Within that folder I add another folder `DotFiles` and
that will be my location of my dotfiles. 

So all files that live directly within my home, will be moved into the
mentioned folder. Within home, I will keep a symbolic link which
points to the moved file. 

There are also files, that live within a sub folder underneath the
home directory. This substructure is mirrored within `DotFiles` so
that it could easily be shifted onto a new system without having a to
remember all the sub structure to be created within home.

Git
---

The whole folder `DotFiles` is under version control of git. I use
multiple branches within that git repository. These are:

  * master
  * development
  * incoming
  * release_github
  * feature_various 
  
I actually are not quite sure, if I really need to have that many
branches, but for now it's works for me. So I use the incoming branch
for adding new config files. I use that branch to freshen up the
configuration before merge them into the development. I branch of the
development into feature branches for developing new
configurations. The `release_github` branch is used to release the
stuff into github. Actually that is one branch that might be not
needed. And also the `master` branch might be unnecessary. 

Scripts
-------

So the last thing I would leave here with you is an explanation of the
[dotAdd.sh](https://github.com/frosch03/dotfiles/blob/release_github/bin/dotAdd.sh)
script. I use that script to mirror the folder structure within my
home, of where a dot file lives. The script extracts the relative path
to the dotfile. It then checks, whether that folder structure is
present in the target folder (`DotFiles`). If that structure isn't
there yet, it's created and otherwise it's just used.

Once the remote folder structure is set up, the dotfile is moved from
the home into the target folder. Last but not least, the symbolic link
is put where the configuration file once was. 

So with that script, it's just a single call to add another
configuration under version control. For example to add my `xmonad.hs`
under version control I have just to call:

``` shell
dotAdd.sh .xmonad/xmonad.hs
```

That will leave a symbolic link instead of xmonad's configuration. The
configuration itself is moved into the target folder. Within the
script, the target folder can be configured.

Closing
-------

And that's basically it. Dotfiles are moved into a different
folder. That folder is under version control and for simplicity there
is script that does the whole work for myself. 

Feel free to contact me if you want me to clarify on something.
