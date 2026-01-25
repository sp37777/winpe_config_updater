unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniFiles;

type
  { TForm1 }
  TForm1 = class(TForm)
    BtnSave: TButton;
    CheckGroup1: TCheckGroup;
    ScrollBox1: TScrollBox;
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

// LOAD DEFAULTS WHEN APP OPENS ---
procedure TForm1.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  SettingsList: TStringList;
  i: Integer;
  KeyName: string;
begin
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'default_choices.ini');
  SettingsList := TStringList.Create;
  try
    // 1. Grab all keys from the [Settings] section
    Ini.ReadSection('Settings', SettingsList);

    // 2. Clear existing items and prep the CheckGroup
    CheckGroup1.Items.BeginUpdate; // Prevents flickering
    CheckGroup1.Items.Clear;

    for i := 0 to SettingsList.Count - 1 do
    begin
      KeyName := SettingsList[i];
      if IsBoolean(IniValue) then
      begin
        // Add the key name as a checkbox item
        CheckGroup1.Items.Add(KeyName);
        // Set its checked state based on the value in the INI
        CheckGroup1.Checked[i] := Ini.ReadBool('Settings', KeyName, False);
      end;
    end;

    CheckGroup1.Items.EndUpdate;
  finally
    SettingsList.Free;
    Ini.Free;
  end;
end;

function IsBoolean(const AValue: string): Boolean;
var
  LowerVal: string;
begin
  LowerVal := LowerCase(AValue);
  Result := (LowerVal = '0') or (LowerVal = '1') or
            (LowerVal = 'true') or (LowerVal = 'false');
end;

procedure TForm1.SyncUIWithIni(const FileName: string);
var
  Ini: TIniFile;
  Keys: TStringList;
  I: Integer;
  Value: string;
  NewEdit: TEdit;
  NewLabel: TLabel;
begin
  Ini := TIniFile.Create(FileName);
  Keys := TStringList.Create;
  try
    Ini.ReadSection('Tweaks', Keys);
    for I := 0 to Keys.Count - 1 do
    begin
      Value := Ini.ReadString('Tweaks', Keys[I], '');

      if IsBoolean(Value) then
      begin
        // Add to your TCheckGroup
        CheckGroup1.Items.Add(Keys[I]);
        CheckGroup1.Checked[CheckGroup1.Items.Count - 1] := Ini.ReadBool('Tweaks', Keys[I], False);
      end
      else
      begin
        // Create a Label
        NewLabel := TLabel.Create(Self);
        NewLabel.Parent := ScrollBox1; // Use a ScrollBox to manage space
        NewLabel.Caption := Keys[I] + ':';
        NewLabel.Align := alTop;
        NewLabel.Margins.Top := 10;

        // Create an Edit for the string
        NewEdit := TEdit.Create(Self);
        NewEdit.Parent := ScrollBox1;
        NewEdit.Text := Value;
        NewEdit.Name := 'edt_' + Keys[I]; // Name it so you can find it later
        NewEdit.Align := alTop;
      end;
    end;
  finally
    Keys.Free;
    Ini.Free;
  end;
end;

// SAVE CHOICES AND EXIT ---
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
    Ini.WriteBool('Tweaks', 'DisableUIBeatification', DisableUIBeatification.Checked);

    // Save to physical disk
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;

  // Terminate the application
  Application.Terminate;
end;

end.
