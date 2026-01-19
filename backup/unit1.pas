unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles;

type
  { TForm1 }
  TForm1 = class(TForm)
    BtnSave: TButton;
    ChkUpdate: TCheckBox;
    ChkDefender: TCheckBox;
    EditUsername: TEdit;
    procedure BtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

// --- 1. LOAD DEFAULTS WHEN APP OPENS ---
procedure TForm1.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  DefaultPath: string;
begin
  // Look for defaults.ini in the same folder as the exe
  DefaultPath := ExtractFilePath(ParamStr(0)) + 'defaults.ini';

  Ini := TIniFile.Create(DefaultPath);
  try
    // Read values: if the file/key doesn't exist, it uses the 3rd parameter
    ChkDefender.Checked := Ini.ReadBool('Settings', 'InstallDrivers', False);
    ChkUpdate.Checked := Ini.ReadBool('Settings', 'ApplyUpdates', False);
    EditUsername.Text       := Ini.ReadString('User', 'UserName', 'DefaultUser');
  finally
    Ini.Free;
  end;
end;

// --- 2. SAVE CHOICES AND EXIT ---
procedure TForm1.BtnSaveClick(Sender: TObject);
var
  Ini: TIniFile;
  USBPath, FinalFilePath: string;
begin
  if ChkDefender = nil then
  begin
    ShowMessage('Error: ChkDefender is Nil! The component is not connected to the form.');
    Exit;
  end;
  // Get the drive letter (e.g., "D:")
  USBPath := ExtractFileDrive(ParamStr(0));

  // If for some reason ParamStr(0) fails (unlikely), fallback to current dir
  if USBPath = '' then USBPath := ExtractFileDrive(GetCurrentDir);

  FinalFilePath := IncludeTrailingPathDelimiter(USBPath) + 'final_settings.ini';

  Ini := TIniFile.Create(FinalFilePath);
  try
    // Write current UI state
    Ini.WriteString('User', 'UserName', EditUsername.Text);
    Ini.WriteBool('Settings', 'InstallDrivers', ChkDefender.Checked);
    Ini.WriteBool('Settings', 'ApplyUpdates', ChkUpdate.Checked);

    // Save to physical disk
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;

  // Terminate the application
  Application.Terminate;
end;

end.
