unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles;

type
  { TForm1 }
  TForm1 = class(TForm)
    BtnSave: TButton;
    DisableCoreIsolation: TCheckBox;
    DisableEncryption: TCheckBox;
    DisableLastAccessTime: TCheckBox;
    DisableUAC: TCheckBox;
    DisableUpdates: TCheckBox;
    DisableDefender: TCheckBox;
    UserName: TEdit;
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
  DefaultPath := ExtractFilePath(ParamStr(0)) + 'default_choices.ini';

  Ini := TIniFile.Create(DefaultPath);
  try
    // Read values: if the file/key doesn't exist, it uses the 3rd parameter
    UserName.Text := Ini.ReadString('User', 'UserName', 'User');
    DisableDefender.Checked := Ini.ReadBool('Tweaks', 'DisableDefender', False);
    DisableUpdates.Checked := Ini.ReadBool('Tweaks', 'DisableUpdates', False);
    DisableUAC.Checked := Ini.ReadBool('Tweaks', 'DisableUAC', False);
    DisableLastAccessTime.Checked := Ini.ReadBool('Tweaks', 'DisableLastAccessTime', False);
    DisableEncryption.Checked := Ini.ReadBool('Tweaks', 'DisableEncryption', False);
    DisableCoreIsolation.Checked := Ini.ReadBool('Tweaks', 'DisableCoreIsolation', False);
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
  if DisableDefender = nil then
  begin
    ShowMessage('Error: ChkDefender is Nil! The component is not connected to the form.');
    Exit;
  end;
  // Get the drive letter (e.g., "D:")
  USBPath := ExtractFilePath(ParamStr(0));

  // If for some reason ParamStr(0) fails (unlikely), fallback to current dir
  if USBPath = '' then USBPath := ExtractFilePath(GetCurrentDir);

  FinalFilePath := IncludeTrailingPathDelimiter(USBPath) + 'user_choices.ini';

  Ini := TIniFile.Create(FinalFilePath);
  try
    // Write current UI state
    Ini.WriteString('User', 'UserName', UserName.Text);
    Ini.WriteBool('Tweaks', 'DisableDefender', DisableDefender.Checked);
    Ini.WriteBool('Tweaks', 'DisableUpdates', DisableUpdates.Checked);
    Ini.WriteBool('Tweaks', 'DisableUAC', DisableUAC.Checked);
    Ini.WriteBool('Tweaks', 'DisableLastAccessTime', DisableLastAccessTime.Checked);
    Ini.WriteBool('Tweaks', 'DisableEncryption', DisableEncryption.Checked);
    Ini.WriteBool('Tweaks', 'DisableCoreIsolation', DisableCoreIsolation.Checked);

    // Save to physical disk
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;

  // Terminate the application
  Application.Terminate;
end;

end.
