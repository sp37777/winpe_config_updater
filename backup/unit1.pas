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
    Label1: TLabel;
    procedure BtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SyncConfig(IsLoading: Boolean);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ This function handles BOTH reading and writing to avoid duplicating logic }
procedure TForm1.SyncConfig(IsLoading: Boolean);
var
  Ini: TIniFile;
  ConfigPath: string;
begin
  // Set path to the same folder as the EXE
  ConfigPath := ExtractFilePath(ParamStr(0)) + 'settings.ini';

  Ini := TIniFile.Create(ConfigPath);
  try
    if IsLoading then
    begin
      // READING FROM INI TO UI
      EditUsername.Text := Ini.ReadString('UserConfig', 'UserName', 'Administrator');
      ChkUpdate.Checked := Ini.ReadBool('Tweaks', 'DisableUpdates', False);
      ChkDrivers.Checked := Ini.ReadBool('Tweaks', 'InstallDrivers', True);
    end
    else
    begin
      // WRITING FROM UI TO INI
      Ini.WriteString('UserConfig', 'UserName', EditUsername.Text);
      Ini.WriteBool('Tweaks', 'DisableUpdates', ChkUpdate.Checked);
      Ini.WriteBool('Tweaks', 'InstallDrivers', ChkDrivers.Checked);
      ShowMessage('Configuration Saved to ' + ConfigPath);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Load defaults when the app starts
  SyncConfig(True);
end;

procedure TForm1.BtnSaveClick(Sender: TObject);
begin
  SyncConfig(False);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin

end;


end.
