unit UnitMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    EditSizeFile: TEdit;
    Label1: TLabel;
    UpDownSizeFile: TUpDown;
    ComboBoxTypeSizeFile: TComboBox;
    ListBox: TListBox;
    Label2: TLabel;
    LabelAdd: TLabel;
    LabelDelete: TLabel;
    LabelClear: TLabel;
    LabelLoadRef: TLabel;
    LabelSaveRef: TLabel;
    Bevel: TBevel;
    Label3: TLabel;
    EditForm: TEdit;
    EditTo: TEdit;
    Label4: TLabel;
    UpDownForm: TUpDown;
    Label5: TLabel;
    UpDownTo: TUpDown;
    ButtonStart: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SaveDialogGen: TSaveDialog;
    PanelProgress: TPanel;
    ProgressBar: TProgressBar;
    LabelTitleProgress: TLabel;
    ButtonStop: TButton;
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure LabelAddClick(Sender: TObject);
    procedure LabelDeleteClick(Sender: TObject);
    procedure LabelClearClick(Sender: TObject);
    procedure LabelLoadRefClick(Sender: TObject);
    procedure LabelSaveRefClick(Sender: TObject);
    procedure ComboBoxTypeSizeFileChange(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure SetShowCoutRef;
    procedure SetUpDownSizeFile;
  private

    procedure SetBeginProgress;
    procedure SetEndProgress;

  private {событие потока}
    procedure localGenOnBegin(Sender: TObject);
    procedure localGenOnEnd(Sender: TObject);
    procedure localGenOnTerminate(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.Message,
  Lb.SysUtils,
  Lb.GenFile;

var
  localGen: TGenFile = nil;

procedure TMainForm.FormShow(Sender: TObject);
var
  xInitialDir: String;
begin
  Self.Caption := 'Программа – генерации файла';

  xInitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog.InitialDir := xInitialDir;
  SaveDialog.InitialDir := xInitialDir;

  Self.SetUpDownSizeFile;
  Self.SetShowCoutRef;

  PanelProgress.Visible := False;
end;

procedure TMainForm.LabelAddClick(Sender: TObject);
var
  xValue: array [0..1] of String;
begin
  if not InputQuery('Добавить', ['Текст:'],xValue) then
    Exit;
  ListBox.Items.Add(xValue[0]);
  SetShowCoutRef;
end;

procedure TMainForm.LabelDeleteClick(Sender: TObject);
var
  xS: String;
  xValue: String;
  xIndex: Integer;
begin
  xIndex := ListBox.ItemIndex;
  if xIndex >= 0 then
  begin
    xValue := ListBox.Items[xIndex];
    xS := 'Действительно хотите удалите: row_id = %d; value = "%s"';
    xS := Format(xS,[xIndex,xValue]);
    if TMessageApp.GetInfoMessageDlg(xS,'Действие') then
    begin
      ListBox.Items.Delete(xIndex);
      SetShowCoutRef;
    end;
  end;
end;

procedure TMainForm.LabelClearClick(Sender: TObject);
var
  xS: String;
begin
  if ListBox.Items.Count > 0 then
  begin
    xS := 'Очистить справочник';
    if TMessageApp.GetInfoMessageDlg(xS,'Действие') then
    begin
      ListBox.Items.Clear;
      SetShowCoutRef;
    end;
  end;
end;

procedure TMainForm.LabelLoadRefClick(Sender: TObject);
var
  xS: String;
  xFileName: String;
begin
  if OpenDialog.Execute then
  begin
    ListBox.Items.Clear;
    xFileName := OpenDialog.FileName;
    if FileExists(xFileName) then
    begin
      try
        ListBox.Items.LoadFromFile(xFileName,TEncoding.UTF8);
      except
        on E : Exception do
        begin
          xS := 'Error Message: ' + E.Message + sLineBreak;
          xS := xS + 'Текстовый файл – должен быть в формате  UTF8';
          TMessageApp.SetErrorUser(xS,'Ошибка чтение');
        end;
      end;
      SetShowCoutRef;
    end;
  end;
end;

procedure TMainForm.LabelSaveRefClick(Sender: TObject);
var
  xFileName: String;
begin
  if SaveDialog.Execute then
  begin
    xFileName := SaveDialog.FileName;
    ListBox.Items.SaveToFile(xFileName,TEncoding.UTF8);
    SetShowCoutRef;
  end;
end;

procedure TMainForm.SetShowCoutRef;
begin
  Label2.Caption := 'Словарь для генерации файла: [' + IntToStr(ListBox.Items.Count) + ']';
end;

procedure TMainForm.SetUpDownSizeFile;
var
  xIndex: Integer;
begin
  xIndex := ComboBoxTypeSizeFile.ItemIndex;
  case xIndex of
    0: begin
      UpDownSizeFile.Min := 10;
      UpDownSizeFile.Max := 1024;
      UpDownSizeFile.Position := 10;
    end;

    1: begin
      UpDownSizeFile.Min := 1;
      UpDownSizeFile.Max := 10;
      UpDownSizeFile.Position := 1;
    end;
  end;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin

  if ListBox.Items.Count = 0 then
    raise Exception.Create('Error Message: Справочник пустой');

  if SaveDialogGen.Execute then
  begin
    localGen := TGenFile.Create;
    with localGen do
    begin
      SizeFile     := UpDownSizeFile.Position;
      TypeSizeFile := ComboBoxTypeSizeFile.ItemIndex;
      NumberForm   := UpDownForm.Position;
      NumberTo     := UpDownTo.Position;
      References.Assign(ListBox.Items);
      FileName     := SaveDialogGen.FileName;
    end;
    localGen.OnBegin     := localGenOnBegin;
    localGen.OnEnd       := localGenOnEnd;
    localGen.OnTerminate := localGenOnTerminate;
    localGen.Start;
  end;
end;

procedure TMainForm.SetBeginProgress;

  procedure SetEnabledFalse(const AWinControl: TWinControl);
  begin
    AWinControl.Enabled := False;
  end;

begin
  {todo: можно было добавить полу прозрачность}
  ProgressBar.Position := 0;

  SetEnabledFalse(EditSizeFile);
  SetEnabledFalse(UpDownSizeFile);
  SetEnabledFalse(ComboBoxTypeSizeFile);
  SetEnabledFalse(ListBox);
  SetEnabledFalse(EditForm);
  SetEnabledFalse(EditTo);
  SetEnabledFalse(UpDownForm);
  SetEnabledFalse(UpDownTo);
  SetEnabledFalse(ButtonStart);

  ButtonStop.Caption := 'Стоп';
  PanelProgress.Visible := True;

  Timer.Enabled := True;
end;

procedure TMainForm.SetEndProgress;

  procedure SetEnabledTrue(const AWinControl: TWinControl);
  begin
    AWinControl.Enabled := True;
  end;

begin
  SetEnabledTrue(EditSizeFile);
  SetEnabledTrue(UpDownSizeFile);
  SetEnabledTrue(ComboBoxTypeSizeFile);
  SetEnabledTrue(ListBox);
  SetEnabledTrue(EditForm);
  SetEnabledTrue(EditTo);
  SetEnabledTrue(UpDownForm);
  SetEnabledTrue(UpDownTo);
  SetEnabledTrue(ButtonStart);

  PanelProgress.Visible := False;
  Timer.Enabled := False;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xS: String;
  xProgress, xMax, xProcent: Integer;
begin
  if Assigned(localGen) then
  begin
    try
      xProgress := localGen.Progress;
      xMax      := localGen.MaxSize;
      xProcent  := Trunc((xProgress/xMax) * 100);

      xS := 'Значение: ' + IntToStr(xProcent);
      LabelTitleProgress.Caption := xS + '% ' + Format('(%d/%d)',[xProgress,xMax]);
      ProgressBar.Position := xProcent;

    except
      SetEndProgress;
    end;
  end;
end;

procedure TMainForm.ComboBoxTypeSizeFileChange(Sender: TObject);
begin
  Self.SetUpDownSizeFile;
end;

procedure TMainForm.localGenOnBegin(Sender: TObject);
begin
  if Assigned(localGen) then
  begin
    try
      SetBeginProgress;
    except
      localGen := nil;
    end;
  end;
end;

procedure TMainForm.localGenOnEnd(Sender: TObject);
begin
  if Assigned(localGen) then
  begin
    try
      ProgressBar.Position := 100;
      LabelTitleProgress.Caption := 'Создание файла завершено';
      ButtonStop.Caption := 'Закрыть';
      Timer.Enabled := False;
    except
      localGen := nil;
    end;
  end;
end;

procedure TMainForm.localGenOnTerminate(Sender: TObject);
begin
  localGen := nil;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  if Assigned(localGen) then
  begin
    try
      localGen.Terminate;
    except
      localGen := nil;
    end;
  end
  else
    SetEndProgress;
end;

end.
