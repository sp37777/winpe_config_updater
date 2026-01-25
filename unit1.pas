unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IniFiles, LCLIntf, LCLType, Windows; // Standard Lazarus units (No Winapi prefix)

type
  { TForm1 }
  TForm1 = class(TForm)
    BtnSave: TButton;
    CheckGroup1: TCheckGroup;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  private
    procedure ClearDynamicComponents;
    function IsBoolean(const AValue: string): Boolean;
    function SafeName(const AKey: string): string;
    function UnsafeName(const AName: string): string;
  public
    procedure SyncUIWithIni(const FileName: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm} // Lazarus uses .lfm instead of .dfm

{ TForm1 }

function TForm1.IsBoolean(const AValue: string): Boolean;
var
  L: string;
begin
  L := LowerCase(Trim(AValue));
  Result := (L = '0') or (L = '1') or (L = 'true') or (L = 'false') or (L = 'yes') or (L = 'no');
end;

function TForm1.SafeName(const AKey: string): string;
begin
  // Lazarus component names cannot have spaces
  Result := StringReplace(AKey, ' ', '_', [rfReplaceAll]);
end;

function TForm1.UnsafeName(const AName: string): string;
begin
  // Remove 'edt_' and change underscores back to spaces
  Result := StringReplace(Copy(AName, 5, Length(AName)), '_', ' ', [rfReplaceAll]);
end;

procedure TForm1.ClearDynamicComponents;
var
  I: Integer;
begin
  // Loop backwards when freeing components
  for I := Self.ComponentCount - 1 downto 0 do
  begin
    if (Pos('edt_', Components[I].Name) = 1) or (Pos('lbl_', Components[I].Name) = 1) then
      Components[I].Free;
  end;
end;

procedure TForm1.SyncUIWithIni(const FileName: string);
var
  Ini: TIniFile;
  Sections, Keys: TStringList;
  S, K: Integer;
  Value, SectionName, KeyName: string;
  NewEdit: TEdit;
  NewLabel: TLabel;
  LastTop: Integer;
begin
  ClearDynamicComponents;
  CheckGroup1.Items.Clear;
  LastTop := 10;

  if not FileExists(FileName) then Exit;

  Ini := TIniFile.Create(FileName);
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    // 1. Get a list of all section names ([User], [Tweaks], etc.)
    Ini.ReadSections(Sections);

    // 2. Loop through each section
    for S := 0 to Sections.Count - 1 do
    begin
      SectionName := Sections[S];
      Keys.Clear;
      Ini.ReadSection(SectionName, Keys);

      // 3. Loop through each key in the current section
      for K := 0 to Keys.Count - 1 do
      begin
        KeyName := Keys[K];
        Value := Ini.ReadString(SectionName, KeyName, '');

        if IsBoolean(Value) then
        begin
          CheckGroup1.Items.Add(KeyName);
          CheckGroup1.Checked[CheckGroup1.Items.Count - 1] := Ini.ReadBool(SectionName, KeyName, False);
        end
        else
        begin
          // Create Label for the string
          NewLabel := TLabel.Create(Self);
          NewLabel.Parent := ScrollBox1;
          NewLabel.Name := 'lbl_' + SafeName(KeyName);
          NewLabel.Caption := KeyName + ':';
          NewLabel.Left := 10;
          NewLabel.Top := LastTop;

          // Create Edit for the string
          NewEdit := TEdit.Create(Self);
          NewEdit.Parent := ScrollBox1;
          NewEdit.Name := 'edt_' + SafeName(KeyName);
          NewEdit.Text := Value;
          NewEdit.Left := 10;
          NewEdit.Top := LastTop + 18;
          NewEdit.Width := ScrollBox1.ClientWidth - 25;
          NewEdit.Anchors := [akLeft, akTop, akRight];

          LastTop := NewEdit.Top + NewEdit.Height + 15;
        end;
      end;
    end;
  finally
    Sections.Free;
    Keys.Free;
    Ini.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SyncUIWithIni(ExtractFilePath(ParamStr(0)) + 'default_choices.ini');
end;

procedure TForm1.BtnSaveClick(Sender: TObject);
var
  Ini: TIniFile;
  I: Integer;
begin
  // We save everything into one flat section [Setup] to make it easy for the installer
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'user_choices.ini');
  try
    // Save all booleans from CheckGroup
    for I := 0 to CheckGroup1.Items.Count - 1 do
    begin
      Ini.WriteBool('Setup', CheckGroup1.Items[I], CheckGroup1.Checked[I]);
    end;

    // Save all strings from TEdits
    for I := 0 to Self.ComponentCount - 1 do
    begin
      if (Components[I] is TEdit) and (Pos('edt_', Components[I].Name) = 1) then
      begin
        Ini.WriteString('Setup', UnsafeName(Components[I].Name), TEdit(Components[I]).Text);
      end;
    end;
  finally
    Ini.Free;
  end;

  Application.Terminate;
end;

end.
