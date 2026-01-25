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
  Keys: TStringList;
  I: Integer;
  Value: string;
  NewEdit: TEdit;
  NewLabel: TLabel;
  LastTop: Integer;
begin
  ClearDynamicComponents;
  CheckGroup1.Items.Clear;
  LastTop := 10;

  if not FileExists(FileName) then Exit;

  Ini := TIniFile.Create(FileName);
  Keys := TStringList.Create;
  try
    Ini.ReadSection('Tweaks', Keys);
    for I := 0 to Keys.Count - 1 do
    begin
      Value := Ini.ReadString('Tweaks', Keys[I], '');

      if IsBoolean(Value) then
      begin
        CheckGroup1.Items.Add(Keys[I]);
        CheckGroup1.Checked[CheckGroup1.Items.Count - 1] := Ini.ReadBool('Tweaks', Keys[I], False);
      end
      else
      begin
        // Create Label
        NewLabel := TLabel.Create(Self);
        NewLabel.Parent := ScrollBox1;
        NewLabel.Name := 'lbl_' + SafeName(Keys[I]);
        NewLabel.Caption := Keys[I] + ':';
        NewLabel.Left := 10;
        NewLabel.Top := LastTop;

        // Create Edit
        NewEdit := TEdit.Create(Self);
        NewEdit.Parent := ScrollBox1;
        NewEdit.Name := 'edt_' + SafeName(Keys[I]);
        NewEdit.Text := Value;
        NewEdit.Left := 10;
        NewEdit.Top := LastTop + 18;
        NewEdit.Width := ScrollBox1.Width - 40;
        NewEdit.Anchors := [akLeft, akTop, akRight];

        LastTop := NewEdit.Top + NewEdit.Height + 15;
      end;
    end;
  finally
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
  ActualKey: string;
begin
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'user_choices.ini');
  try
    // 1. Save Booleans
    for I := 0 to CheckGroup1.Items.Count - 1 do
    begin
      Ini.WriteBool('Tweaks', CheckGroup1.Items[I], CheckGroup1.Checked[I]);
    end;

    // 2. Save Strings
    for I := 0 to Self.ComponentCount - 1 do
    begin
      if (Components[I] is TEdit) and (Pos('edt_', Components[I].Name) = 1) then
      begin
        ActualKey := UnsafeName(Components[I].Name);
        Ini.WriteString('Tweaks', ActualKey, TEdit(Components[I]).Text);
      end;
    end;
  finally
    Ini.Free;
  end;

  Application.Terminate; // Close and proceed with installation
end;

end.
