unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IniFiles, ComCtrls;

type
  { TForm1 }
  TForm1 = class(TForm)
    BtnSave: TButton;
    EditSearch: TEdit;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure EditSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    LastTopPosition: Integer;
    procedure CreateDynamicField(const AName, AValue: string; IsHeader: Boolean = False);
    function SafeName(const AKey: string): string;
    function UnsafeName(const AName: string): string;
    function GetConfigPath: string;
  public
    procedure SyncUIWithIni(const FileName: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function TForm1.SafeName(const AKey: string): string;
begin
  Result := StringReplace(AKey, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '.', '_', [rfReplaceAll]);
end;

function TForm1.UnsafeName(const AName: string): string;
var
  BaseName: string;
begin
  // dyn_ed_ or dyn_cb_ are 7 chars
  BaseName := Copy(AName, 8, Length(AName));
  Result := StringReplace(BaseName, '_', ' ', [rfReplaceAll]);
end;

function TForm1.GetConfigPath: string;
var
  SR: TSearchRec;
  ExeDir, ParamVal: string;
begin
  Result := '';
  ExeDir := ExtractFilePath(ParamStr(0));

  if Application.HasOption('c', 'config') then
  begin
    ParamVal := Application.GetOptionValue('c', 'config');

    // Check if it's a full path already
    if FileExists(ParamVal) then
      Result := ParamVal
    // Check if it's a relative path or filename in AppDir
    else if FileExists(ExeDir + ParamVal) then
      Result := ExeDir + ParamVal;
  end;

  // Search for *default*.ini if no valid path provided yet
  if (Result = '') or (not FileExists(Result)) then
  begin
    if FindFirst(ExeDir + '*default*.ini', faAnyFile, SR) = 0 then
    begin
      Result := ExeDir + SR.Name;
      FindClose(SR);
    end;
  end;
end;

procedure TForm1.CreateDynamicField(const AName, AValue: string; IsHeader: Boolean = False);
var
  Lab: TLabel;
  Ed: TEdit;
  Cb: TCheckBox;
  CleanName: string;
begin
  if IsHeader then
  begin
    Lab := TLabel.Create(Self);
    Lab.Parent := ScrollBox1;
    Lab.Caption := '--- ' + AName + ' ---';
    Lab.Font.Style := [fsBold];
    Lab.Top := LastTopPosition + 10;
    Lab.Left := 10;
    LastTopPosition := Lab.Top + 30;
    Exit;
  end;

  CleanName := SafeName(AName);

  if SameText(AValue, 'True') or SameText(AValue, 'False') or (AValue = '1') or (AValue = '0') then
  begin
    Cb := TCheckBox.Create(Self);
    Cb.Parent := ScrollBox1;
    Cb.Name := 'dyn_cb_' + CleanName;
    Cb.Caption := AName;
    Cb.Checked := StrToBoolDef(AValue, False);
    Cb.Top := LastTopPosition;
    Cb.Left := 10;
  end
  else
  begin
    Lab := TLabel.Create(Self);
    Lab.Parent := ScrollBox1;
    Lab.Caption := AName;
    Lab.Top := LastTopPosition;
    Lab.Left := 10;

    Ed := TEdit.Create(Self);
    Ed.Parent := ScrollBox1;
    Ed.Name := 'dyn_ed_' + CleanName;
    Ed.Text := AValue;
    Ed.Top := Lab.Top - 4;
    Ed.Left := 150;
    Ed.Width := 200;
    Ed.Anchors := [akTop, akLeft, akRight];
  end;
  LastTopPosition := LastTopPosition + 35;
end;

procedure TForm1.SyncUIWithIni(const FileName: string);
var
  Ini: TIniFile;
  Secs, Keys: TStringList;
  i, j: Integer;
  FoundUser: Boolean;
begin
  for i := ScrollBox1.ControlCount - 1 downto 0 do ScrollBox1.Controls[i].Free;
  LastTopPosition := 10;
  FoundUser := False;

  if FileExists(FileName) then
  begin
    StatusBar1.SimpleText := ' Config: ' + FileName;
    Ini := TIniFile.Create(FileName);
    Secs := TStringList.Create;
    Keys := TStringList.Create;
    try
      Ini.ReadSections(Secs);
      for i := 0 to Secs.Count - 1 do
      begin
        CreateDynamicField(Secs[i], '', True);
        Keys.Clear;
        Ini.ReadSection(Secs[i], Keys);
        for j := 0 to Keys.Count - 1 do
        begin
          if SameText(Keys[j], 'UserName') then FoundUser := True;
          CreateDynamicField(Keys[j], Ini.ReadString(Secs[i], Keys[j], ''));
        end;
      end;
    finally
      Secs.Free; Keys.Free; Ini.Free;
    end;
  end
  else
    StatusBar1.SimpleText := ' No config found. Using defaults.';

  if not FoundUser then
  begin
    CreateDynamicField('Identity', '', True);
    CreateDynamicField('UserName', 'User');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SyncUIWithIni(GetConfigPath);
end;

procedure TForm1.EditSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  C: TControl;
  Txt: string;
  CurTop: Integer;
begin
  Txt := LowerCase(Trim(EditSearch.Text));
  CurTop := 10;
  ScrollBox1.DisableAlign;
  try
    for i := 0 to ScrollBox1.ControlCount - 1 do
    begin
      C := ScrollBox1.Controls[i];
      C.Visible := (Txt = '') or (Pos(Txt, LowerCase(C.Name)) > 0) or
                   ((C is TLabel) and (Pos(Txt, LowerCase(TLabel(C).Caption)) > 0)) or
                   ((C is TCheckBox) and (Pos(Txt, LowerCase(TCheckBox(C).Caption)) > 0));

      if C.Visible then
      begin
        if (C is TLabel) or (C is TCheckBox) then
        begin
          C.Top := CurTop;
          CurTop := CurTop + 35;
        end
        else if C is TEdit then
          C.Top := CurTop - 35 - 4;
      end;
    end;
  finally
    ScrollBox1.EnableAlign;
  end;
end;

procedure TForm1.BtnSaveClick(Sender: TObject);
var
  Ini: TIniFile;
  i: Integer;
  C: TControl;
  Path: string;
begin
  Path := ExtractFilePath(ParamStr(0)) + 'user_choices.ini';
  Ini := TIniFile.Create(Path);
  try
    for i := 0 to ScrollBox1.ControlCount - 1 do
    begin
      C := ScrollBox1.Controls[i];
      if (C is TEdit) and (Pos('dyn_ed_', C.Name) = 1) then
        Ini.WriteString('Setup', UnsafeName(C.Name), TEdit(C).Text)
      else if (C is TCheckBox) and (Pos('dyn_cb_', C.Name) = 1) then
        Ini.WriteBool('Setup', UnsafeName(C.Name), TCheckBox(C).Checked);
    end;
  finally
    Ini.Free;
  end;
  ShowMessage('Saved to ' + Path);
  Application.Terminate;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := QuestionDlg('Exit', 'Close without saving?', mtConfirmation, [mrYes, mrNo], 0) = mrYes;
end;

end.
