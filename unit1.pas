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

procedure TForm1.BtnSaveClick(Sender: TObject);
var
  Ini: TIniFile;
  I: Integer;
  KeyName: string;
  DynamicEdit: TComponent;
begin
  // Initialize Ini link (ensure you have the correct path to your ini)
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'defaults.ini');
  try
    { 1. Save Boolean Tweaks from TCheckGroup }
    for I := 0 to CheckGroup1.Items.Count - 1 do
    begin
      KeyName := CheckGroup1.Items[I];
      // Save '1' for checked, '0' for unchecked
      if CheckGroup1.Checked[I] then
        Ini.WriteString('Tweaks', KeyName, '1')
      else
        Ini.WriteString('Tweaks', KeyName, '0');
    end;

    { 2. Save String Values from Dynamic TEdits }
    // We iterate through the keys we know are strings, or search by naming convention
    // Here we search through all items in the Ini section to find corresponding Edits
    for I := 0 to CheckGroup1.Items.Count + 20 do // Safety buffer or use a list of string keys
    begin
      { Note: A cleaner way is to keep a TStringList of 'StringKeys'
        identified during the Populate phase. For this example,
        we check for the component by name. }
    end;

    // Alternative: Iterate through the Form's components to find our custom Edits
    for I := 0 to Self.ComponentCount - 1 do
    begin
      if (Components[I] is TEdit) and (Pos('edt_', Components[I].Name) = 1) then
      begin
        // Strip the 'edt_' prefix to get the original INI Key Name
        KeyName := Copy(Components[I].Name, 5, Length(Components[I].Name));
        Ini.WriteString('Tweaks', KeyName, TEdit(Components[I]).Text);
      end;
    end;

    ShowMessage('Settings saved successfully!');
  finally
    Ini.Free;
  end;

  // Terminate the application
  Application.Terminate;
end;

end.
