unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IniFiles, ComCtrls, Buttons, Process, FileUtil, Windows;

type
  { TForm1 }
  TForm1 = class(TForm)
    BtnSave: TButton;
    EditSearch: TEdit;
    ScrollBox1: TScrollBox;
    BtnClearSearch: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure EditSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure BtnClearSearchClick(Sender: TObject);
  private
    LastTopPosition: Integer;
    procedure CreateDynamicField(const AName, AValue: string; IsHeader: Boolean = False);
    function SafeName(const AKey: string): string;
    function UnsafeName(const AName: string): string;
    function GetConfigPath: string;
    procedure CloseFormTimer(Sender: TObject);
    function IsVolumeDirty(const ADrive: string): Boolean;
  public
    procedure SyncUIWithIni(const FileName: string);
    procedure SelfClosingMsg(const ATitle, AMsg: string; ADuration: Integer);
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
  BaseDir, ParamVal: string;
begin
  Result := '';
  BaseDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  if Application.HasOption('c', 'config') then
  begin
    ParamVal := Application.GetOptionValue('c', 'config');
    
    // Check absolute path or path relative to the app folder
    if FileExists(ParamVal) then 
      Result := ParamVal
    else if FileExists(BaseDir + ParamVal) then 
      Result := BaseDir + ParamVal;
  end;

  // Fallback to local default file
  if (Result = '') then
  begin
    if FindFirst(BaseDir + '*default*.ini', faAnyFile, SR) = 0 then
    begin
      Result := BaseDir + SR.Name;
      SysUtils.FindClose(SR); 
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

procedure TForm1.CloseFormTimer(Sender: TObject);
begin
  // 'Sender' is the Timer. Its 'Owner' is the Form created in SelfClosingMsg.
  if (Sender is TTimer) and (TTimer(Sender).Owner is TForm) then
    TForm(TTimer(Sender).Owner).Close;
end;

procedure TForm1.SelfClosingMsg(const ATitle, AMsg: string; ADuration: Integer);
var
  MsgForm: TForm;
  MsgLabel: TLabel;
  Timer: TTimer;
begin
  MsgForm := TForm.Create(nil);
  with MsgForm do
  begin
    BorderStyle := bsDialog;
    Caption := ATitle;
    Position := poScreenCenter;
    Width := 300;
    Height := 100;
    FormStyle := fsStayOnTop;
  end;

  MsgLabel := TLabel.Create(MsgForm);
  with MsgLabel do
  begin
    Parent := MsgForm;
    Align := alClient;
    Alignment := taCenter;
    Layout := tlCenter;
    Caption := AMsg;
  end;

  Timer := TTimer.Create(MsgForm);
  Timer.Interval := ADuration;
  // Now this will compile because it points to a method of TForm1
  Timer.OnTimer := @CloseFormTimer;

  MsgForm.Show;
end;

function TForm1.IsVolumeDirty(const ADrive: string): Boolean;
var
  hDevice: THandle;
  VolumeFlags: DWORD;
  BytesReturned: DWORD;
  DrivePath: string;
begin
  Result := False;
  // Format drive for CreateFile (e.g., \\.\D:)
  DrivePath := '\\.\' + Copy(ADrive, 1, 2);
  
  hDevice := CreateFile(PChar(DrivePath),
    GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0);

  if hDevice <> INVALID_HANDLE_VALUE then
  begin
    try
      // IOCTL_DISK_IS_WRITABLE or checking volume flags
      // For a simpler method in WinPE, we check the Volume Flags
      if DeviceIoControl(hDevice, 
         $00090028, // FSCTL_IS_VOLUME_DIRTY
         nil, 0, 
         @VolumeFlags, SizeOf(VolumeFlags), 
         BytesReturned, nil) then
      begin
        // If the first bit is 1, the volume is dirty
        Result := (VolumeFlags and 1) <> 0;
      end;
    finally
      CloseHandle(hDevice);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Self.Position := poScreenCenter;
  SyncUIWithIni(GetConfigPath);
end;

procedure TForm1.EditSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  C: TControl;
  Txt: string;
  CurTop: Integer;
begin
  // Show the 'X' button only if the search box isn't empty
  BtnClearSearch.Visible := (EditSearch.Text <> '');
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
  SL: TStringList;
  i: Integer;
  C: TControl;
  BaseDir, OutputPath: string;
begin
  // Since we aren't moving to X:, we always use the EXE folder
  BaseDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  OutputPath := BaseDir + 'user_choices.ini';

  SL := TStringList.Create;
  try
    // Section 1: Metadata for verification
    SL.Add('[Metadata]');
    SL.Add('LastSaved=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    SL.Add('');

    // Section 2: User choices
    SL.Add('[Setup]');
    for i := 0 to ScrollBox1.ControlCount - 1 do
    begin
      C := ScrollBox1.Controls[i];
      if (C is TEdit) and (Pos('dyn_ed_', C.Name) = 1) then
        SL.Add(UnsafeName(C.Name) + '=' + TEdit(C).Text)
      else if (C is TCheckBox) and (Pos('dyn_cb_', C.Name) = 1) then
        SL.Add(UnsafeName(C.Name) + '=' + BoolToStr(TCheckBox(C).Checked, '1', '0'));
    end;

    try
      SL.SaveToFile(OutputPath);
      SelfClosingMsg('Success', 'Settings Saved!' + sLineBreak + OutputPath, 3000);
      Application.ProcessMessages;
      Sleep(3000);
      Application.Terminate;
    except
      on E: Exception do
        MessageDlg('Critical Save Error', 
          'Error writing to USB: ' + E.Message, mtError, [mbOK], 0);
    end;
  finally
    SL.Free;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := QuestionDlg('Exit', 'Close without saving?', mtConfirmation, [mrYes, mrNo], 0) = mrYes;
end;

procedure TForm1.BtnClearSearchClick(Sender: TObject);
var
  DummyKey: Word;
  DummyShift: TShiftState;
begin
  EditSearch.Text := '';

  // Manually trigger the KeyUp logic to refresh the layout
  DummyKey := 0;
  DummyShift := [];
  EditSearchKeyUp(EditSearch, DummyKey, DummyShift);

  EditSearch.SetFocus;
end;

end.
