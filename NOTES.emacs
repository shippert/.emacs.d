7/15/2020

* Compile inside of emacs (i.e. emacs -nw on local machine), allows a call
  to ./build.sh and shows and highlights errors.

  Compile: C-x c 
  Go to error: C-c c

* I put my .emacs.d from notebook onto the dmf, and that sets things up
  right.

* TERM=xterm-256color allows full regular emacs color like with the emacs
  window, which is much easier to develop in than the colors I use for
  shells.

* Added this to .emacs.d/init.el:

   (if (not window-system)
       (progn
         (xterm-mouse-mode)))

  ...which allows using the mouse inside an emacs -nw.

* Finally, added 

    (setq desktop-save nil)
    (setq desktop-save-mode nil)

  ...to ~/.emacs.d/local-init.el on the DMF, because using the desktop when
  you fire up a lot of quick emacs -nw in succession is pretty irritating.
  If I want to have a main remote emacs -nw running on the DMF, I might
  want to turn desktopping back on.
  
* With the previous two options, it makes remote emacs development (rather
  than a single local emacs) a lot more viable.  There are benefits still
  to having a local emacs window up via sshfs.  For one thing, copy/paste
  in the remote emacs -nw doesn't really work - I can't figure out how to
  copy stuff to the clipboard inside an emacs -nw.

! However, it looks like the "select, middle-button" copy/paste seems to
  work across windows.  So the clipboard doesn't, but straight
  select/middle does.



