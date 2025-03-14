# Programming an ATF1504 on Windows
The ATF1504 CPLD needs to be programmed before it can be used. It all starts with a .pld file that contains the logic equations. The end-result is a programmed ATF1504 with these logic euqations.


# Hardware needed
I am using the afterburner GAL programmer (https://github.com/Emile666/afterburner), this is my fork from the original repository, with a plcc-44 adapterprint, also found in this repository. I made a couple of modifications to the existing hardware and software, so that it has better control over the programming voltage. The popular TL866 programmer will NOT program an ATF1504, the afterburner hardware is cheap and easy to make.

An alternative is to use the FT232H board with Open-OCD, but I haven't investigated this yet (on the TO-DO list). So the best option for now is to use the Afterburner GAL programmer.

# Software needed

You need to download and install the following programs:
- WinCUPL: download from microchip.com and install it.
- ATMISP v7.3 (for Win 7,8 & 10): download from microchip.com and install it.
- BPv3.XSVFplayer.v1.1.zip: download from code.google.com. The file you need is the svf2xsvf502.exe executable.

I created an afterburner directory with the afterburner.exe file in it. Please check the Afterburner repository for more details on how to get everything to work. I copied the svf2xsvf502.exe program into this directory, so that all the needed executables are here. I also put the .jed, .svf and .xsvf files here.

# Walk-through

Start the WinCUPL program:
- Open the .pld file, example: atf1504_test.pld
- Under Options->Compile->Output Files click the following: Fuse Plot, Equations, List, PLA
- Under General click JEDEC name = PLD name
- Press the button 'Device Dependent Compile'
- Check that no errors are reported. If you get a run-time error 80010108 ('Automation error, the object invoked has disconnected from its clients), then the combination of path-name and file-name is too long. Move your entire directory to the root directory or shorten path & filenames.
- A number of files are being made, of which the .jed and the .fit are the most important ones. Check the .fit file for correct pin-out of the ATF1504, the .jed file is used by the other programs.
- Close the WinCUPL program.

Start the ATMISP program:
 - If no USB cable is found, the RUN button is deactivated. To activate this, create a dummy Device by clicking Edit->Add New Device. Add after device 0 (just click OK). Set Device Name to ATF1504AS, JTAG instruction to Program/Verify and enter JEDEC file to convert, which is the .jed file generated by WinCUPL.
 - In the SVF Setting Dialog, check the 'Write SVF File' checkbox and set SVF Version to Revision C. Leave TCK period to 1 us. 
 - Enter a filename for the SVF File Name and press the RUN button.
 - Check that a .svf file has been generated and copy it to the Afterburner work-directory if it is not already there.
 - Close the ATMISP program and press No for 'Save chain file before exit?'.

SVF2XSVF:
 - Open the command-line shell of Windows11 (cmd) and go to the afterburner work-directory where the .svf file is located.
 - Copy the svf2xsvf502.exe file into your work-directory.
 - On the command line enter: **svf2xsvf502 -i filename.svf -o filename.xsvf**, with filename being the filename of the .svf file generated by ATMISP.
 - Check that a .svxf file is generated and that no errors/warnings are reported.

Afterburner GAL programmer:
 - Insert the ATF1504 in the Afterburner GAL programmer with the plc44 adapter PCB and press the switch (LED goes on).
 - Open a command-line shell and go to the Afterburner directory that contains the afterburner command-line executable.
 - Copy the <filename>.xsvf file into this directory, if it is not already present.
 - On the command-line enter: **afterburner -t ATF1504AS -f filename.xsvf ew**. The afterburner program first erases the ATF1504 and then programs it.
 - Depress the switch again (light goes off) and remove the ATF1504 from its socket.
 
 Enjoy!


