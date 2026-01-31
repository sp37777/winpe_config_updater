unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, IniFiles;

type
  { TForm1 }
  TForm1 = class(TForm)
    BtnSave: TButton;
    EditSearch: TEdit;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  private
    { Internal Layout and State }
    LastTopPosition: Integer;
    procedure CreateDynamicField(const AName, AValue: string; IsHeader: Boolean = False);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure EditSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  // Lazarus component names cannot have spaces or special chars
  Result := StringReplace(AKey, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '.', '_', [rfReplaceAll]);
end;

function TForm1.UnsafeName(const AName: string): string;
var
  BaseName: string;
begin
  // Strip the 'dyn_ed_' or 'dyn_cb_' prefix (7 characters)
  BaseName := Copy(AName, 8, Length(AName));
  Result := StringReplace(BaseName, '_', ' ', [rfReplaceAll]);
end;

function TForm1.GetConfigPath: string;
var
  SR: TSearchRec;
  ExeDir: string;
begin
  Result := '';
  ExeDir := ExtractFilePath(ParamStr(0));

  // 1. Check named parameter
  if Application.HasOption('c', 'config') then
    Result := Application.GetOptionValue('c', 'config');

  // 2. Search for *default*.ini if no valid path yet
  if (Result = '') or (not FileExists(Result)) then
  begin
    if FindFirst(ExeDir + '*default*.ini', faAnyFile, SR) = 0 then
    begin
      Result := ExeDir + SR.Name;
      FindClose(SR);
    end;
  end;

  // 3. Last resort fallback
  if (Result = '') or (not FileExists(Result)) then
    Result := ExeDir + 'settings.ini';
end;

procedure TForm1.EditSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  C: TControl;
  SearchTerm: string;
  CurrentTop: Integer;
  Match: Boolean;
begin
  SearchTerm := Trim(LowerCase(EditSearch.Text));
  CurrentTop := 10;

  // Disable Alignments/Drawing temporarily for speed
  ScrollBox1.DisableAlign;
  try
    for i := 0 to ScrollBox1.ControlCount - 1 do
    begin
      C := ScrollBox1.Controls[i];

      // Check if this specific control matches the search
      // We check Caption for Labels/CheckBoxes and Name for Edits
      Match := (SearchTerm = '') or
               (Pos(SearchTerm, LowerCase(TControl(C).Caption)) > 0) or
               (Pos(SearchTerm, LowerCase(C.Name)) > 0);

      C.Visible := Match;

      // Re-stack visible items so we don't have empty holes
      if C.Visible then
      begin
        // If it's a Label or a standalone Checkbox, it starts a new "row"
        if (C is TLabel) or (C is TCheckBox) then
        begin
           C.Top := CurrentTop;
           // Only increment the 'next' top if it's the end of a row or a header
           if (C is TCheckBox) or (C is TLabel) then CurrentTop := CurrentTop + 35;
        end
        else if (C is TEdit) then
        begin
           // Edits should stay aligned with the Label that was just processed
           C.Top := CurrentTop - 35 - 4;
        end;
      end;
    end;
  finally
    ScrollBox1.EnableAlign;
  end;
end;

procedure TForm1.CreateDynamicField(const AName, AValue: string; IsHeader: Boolean = False);
var
  Lab: TLabel;
  Ed: TEdit;
  Cb: TCheckBox;
  CleanName: string;
  IsBool: Boolean;
begin
  if IsHeader then
  begin
    Lab := TLabel.Create(Self);
    Lab.Parent := ScrollBox1;
    Lab.Caption := AName;
    Lab.Font.Style := [fsBold]; // Make it stand out
    Lab.Top := LastTopPosition + 10;
    Lab.Left := 10;
    LastTopPosition := Lab.Top + 25;
    Exit;
  end;

  CleanName := SafeName(AName);
  // Improved Boolean check for 0/1, True/False
  IsBool := SameText(AValue, 'True') or SameText(AValue, 'False') or
            (AValue = '1') or (AValue = '0');

  if IsBool then
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
  SectionList, KeyList: TStringList;
  i, j: Integer;
  UserNameFound: Boolean;
begin
  // Cleanup old controls
  for i := ScrollBox1.ControlCount - 1 downto 0 do ScrollBox1.Controls[i].Free;

  UserNameFound := False;
  LastTopPosition := 10;
  EditSearch.Text := '';
  // Update Status Bar
  if (FileName <> '') and FileExists(FileName) then
    StatusBar1.SimpleText := ' Loaded: ' + FileName
  else
    StatusBar1.SimpleText := ' No config found. Using factory defaults.';

  if (FileName <> '') and FileExists(FileName) then
  begin
    Ini := TIniFile.Create(FileName);
    SectionList := TStringList.Create;
    KeyList := TStringList.Create;
    try
      Ini.ReadSections(SectionList);
      for i := 0 to SectionList.Count - 1 do
      begin
        // Create the Category Header
        CreateDynamicField(SectionList[i], '', True);

        KeyList.Clear;
        Ini.ReadSection(SectionList[i], KeyList);
        for j := 0 to KeyList.Count - 1 do
        begin
          if SameText(KeyList[j], 'UserName') then UserNameFound := True;
          CreateDynamicField(KeyList[j], Ini.ReadString(SectionList[i], KeyList[j], ''));
        end;
      end;
    finally
      KeyList.Free; SectionList.Free; Ini.Free;
    end;
  end;

  if not UserNameFound then
  begin
    CreateDynamicField('Default Settings', '', True);
    CreateDynamicField('UserName', 'User');
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // QuestionDlg is great for WinPE as it is lightweight
  if QuestionDlg('Exit Confirmation', 'Are you sure you want to exit? Any unsaved changes will be lost.',
     mtConfirmation, [mrYes, mrNo], 0) = mrYes then
  begin
    CanClose := True;
  end
  else
  begin
    CanClose := False;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SyncUIWithIni(GetConfigPath);
end;

procedure TForm1.BtnSaveClick(Sender: TObject);
var
  Ini: TIniFile;
  I: Integer;
  C: TControl;
  OutputPath: string;
begin
  OutputPath := ExtractFilePath(ParamStr(0)) + 'user_choices.ini';
  
  // Create new INI (this overwrites existing)
  Ini := TIniFile.Create(OutputPath);
  try
    // Iterate through everything inside the ScrollBox
    for I := 0 to ScrollBox1.ControlCount - 1 do
    begin
      C := ScrollBox1.Controls[I];
      
      if (C is TEdit) and (Pos('dyn_ed_', C.Name) = 1) then
        Ini.WriteString('Setup', UnsafeName(C.Name), TEdit(C).Text);
        
      if (C is TCheckBox) and (Pos('dyn_cb_', C.Name) = 1) then
        Ini.WriteBool('Setup', UnsafeName(C.Name), TCheckBox(C).Checked);
    end;
  finally
    Ini.Free;
  end;

  ShowMessage('Settings saved to: ' + OutputPath);
  Application.Terminate;
end;

end.
