from tkinter import messagebox, Tk
import sys
import traceback
import os
from tkinter.filedialog import *


def printError(text):
    f = open("error.txt", "w")
    f.write(text)
    f.close()

def printFile(text):
    f = open("response.txt", "w")
    f.write(text)
    f.close()

def askYesOrNo(title, text):
    mbox = messagebox.askyesno(title,
                               text)
    if mbox == True:
        printFile("Yes")
    else:
        printFile("No")

def askYesNoCancel(self, title, text):

    mbox = messagebox.askyesnocancel(self.__dicts.getWordFromCurrentLanguage(title),
                                   self.__dicts.getWordFromCurrentLanguage(text))

    if mbox==True:
       return("Yes")
    elif mbox == False:
       return("No")
    else:
       return("Cancel")


def askForFileName(title, save, initdir):
        if save:
            types =\
            (
                ("VGM Files", "*.vgm"),
                ("All Files", "*.*")
            )
        else:
            types =\
            (
                ("MIDI Files", "*.mid"),
                ("All Files", "*.*")
            )

        if os.path.exists(initdir) == False or initdir == None:
            test = os.getcwd()+os.sep+initdir
            if os.path.exists(test) == False:
                initdir = "*"
            else:
                initdir = test

        if save == True:
            openname = asksaveasfilename(initialdir=initdir,
                                       title=title,
                                       filetypes=types)
        else:
            openname = askopenfilename(initialdir=initdir,
                                       title=title,
                                       filetypes=types)

        printFile(openname)

def askForDir(title, init):

    printFile(askdirectory(initialdir=init,
                            title=title,
                            ))

def displayError(title, message):
    messagebox.showerror(title, message)

if __name__ == "__main__":
   try:
       tk = Tk()
       tk.withdraw()
       tk.overrideredirect(True)
       tk.resizable(False, False)
       tk.geometry("%dx%d+%d+%d" % (1, 1, 1, 1))
       args = sys.argv

       #args.extend(["Error", "Error occured!", ""])

       print(args)
       if len(args) > 1:
          args         = sys.argv
          typeOfWindow = args[1]
          title        = args[2]

          if   typeOfWindow == "YesOrNo":
               text     = args[3]
               askYesOrNo(title, text)

          elif typeOfWindow == "YesNoCancel":
               text     = args[3]
               askYesNoCancel(title, text)

          elif typeOfWindow == "OpenFile":
               initDir   = args[3]
               #if initDir == "*": initDir = False
               askForFileName(title, False, initDir)

          elif typeOfWindow == "SaveFile":
              initDir = args[3]
              #if initDir == "*": initDir = False
              askForFileName(title, True, initDir)

          elif typeOfWindow == "OpenFolder":
              initDir = args[3]
              #if initDir == "*": initDir = False
              askForDir(title, initDir)

          elif typeOfWindow == "Error":
              text = args[3]
              displayError(title, text)

   except Exception as e:
       with open("error.txt", "w") as file:
            traceback.print_exc(file=file)