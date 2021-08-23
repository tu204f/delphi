unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.EditBox,
  FMX.NumberBox, FMX.Objects;

type
  TMainForm = class(TForm)
    TabControl: TTabControl;
    TabItemSource: TTabItem;
    TabItemVector: TTabItem;
    LayoutSource: TLayout;
    LayoutVector: TLayout;
    ButtonLoad: TButton;
    NumberBox: TNumberBox;
    ButtonShow: TButton;
    NumberBoxCountCandel: TNumberBox;
    TextCountCandel: TText;
    Text1: TText;
    NumberBoxCountResult: TNumberBox;
    TabItem1: TTabItem;
    LayoutAnalizVector: TLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    NumberBox1: TNumberBox;
    Timer: TTimer;
    Button2: TButton;
    Text2: TText;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonShowClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure SetInitializationSource;
    procedure SetFinalizationSource;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  UnitSourceDataFrame,
  Lb.Candel.Source,
  Lb.Candel.Blocks,
  UnitParamVectorFrame;

var
  localCandelFrame: TSourceDataFrame = nil;
  localVectorFrame: TSourceDataFrame = nil;
  localSourceCandel: TSourceCandel = nil;
  localBlock1: TBlock = nil;
  localBlock2: TBlock = nil;
  localParamVectorFrame: TParamVectorFrame = nil;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Преобразование:';
  Self.SetInitializationSource;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  xIndexBegin, xCountCandel, xCountResult: Integer;
begin
  xIndexBegin := Trunc(NumberBox1.Value);
  xCountCandel := Trunc(NumberBoxCountCandel.Value);
  xCountResult := Trunc(NumberBoxCountResult.Value);
  if localSourceCandel.SetSelected(xIndexBegin,xCountCandel,xCountResult,localBlock2.Sources) then
  begin
    localBlock2.SetFormationVector;
    localParamVectorFrame.SetShowBlock(localBlock1,localBlock2);
  end;
end;

var
  localIndexBegin : Integer = 0;
  localCountCandel: Integer = 0;
  localCountResult: Integer = 0;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  localCountCandel := Trunc(NumberBoxCountCandel.Value);
  localCountResult := Trunc(NumberBoxCountResult.Value);
  if localSourceCandel.SetSelected(localIndexBegin,localCountCandel,localCountResult,localBlock2.Sources) then
  begin
    localBlock2.SetFormationVector;
    localParamVectorFrame.SetShowBlock(localBlock1,localBlock2);
    if GetSameBlock(localBlock1,localBlock2) then
      Text2.Text := 'Совпадение: '
    else
      Text2.Text := 'Нет совпадение: '
  end;

  Inc(localIndexBegin);
  if localIndexBegin >= localSourceCandel.Candels.Count  then
  begin

    Timer.Enabled := False;
  end;
  NumberBox1.Value := localIndexBegin;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  localIndexBegin := 0;
  Timer.Enabled := True;
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
var
  xFileName: String;
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    xFileName := ExtractFilePath(ParamStr(0)) + 'export.csv';
    xStr.LoadFromFile(xFileName);
    if Assigned(localSourceCandel) then
    begin
      localSourceCandel.SetParserCandels(xStr);
      NumberBox.Min := 0;
      NumberBox.Max := localSourceCandel.Candels.Count - 1;


      NumberBox1.Min := 0;
      NumberBox1.Max := localSourceCandel.Candels.Count - 1;

      Self.Caption := 'Загружено: ' + localSourceCandel.Candels.Count.ToString + ' свечей';
    end;
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TMainForm.ButtonShowClick(Sender: TObject);
var
  xIndexBegin, xCountCandel, xCountResult: Integer;
begin
  xIndexBegin := Trunc(NumberBox.Value);
  xCountCandel := Trunc(NumberBoxCountCandel.Value);
  xCountResult := Trunc(NumberBoxCountResult.Value);
  if localSourceCandel.SetSelected(xIndexBegin,xCountCandel,xCountResult,localBlock1.Sources) then
    localCandelFrame.SetShowBlock(xCountCandel,xCountResult);
  localBlock1.SetFormationVector;
  localVectorFrame.SetShowBlock(xCountCandel,xCountResult,False)
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.SetFinalizationSource;
end;

procedure TMainForm.SetInitializationSource;
begin
  // -----------------------------------------------
  localCandelFrame := TSourceDataFrame.Create(LayoutSource);
  localCandelFrame.Parent := LayoutSource;
  localCandelFrame.Align := TAlignLayout.Client;
  // -----------------------------------------------
  localVectorFrame := TSourceDataFrame.Create(LayoutVector);
  localVectorFrame.Parent := LayoutVector;
  localVectorFrame.Align := TAlignLayout.Client;
  // -----------------------------------------------
  localParamVectorFrame := TParamVectorFrame.Create(Layout2);
  localParamVectorFrame.Parent := Layout2;
  localParamVectorFrame.Align := TAlignLayout.Client;
  // -----------------------------------------------
  localSourceCandel := TSourceCandel.Create;
  localBlock1 := TBlock.Create;
  localBlock2 := TBlock.Create;

  localCandelFrame.Block := localBlock1;
  localVectorFrame.Block := localBlock1;
end;

procedure TMainForm.SetFinalizationSource;
begin
  FreeAndNil(localBlock2);
  FreeAndNil(localBlock1);
  FreeAndNil(localSourceCandel);
  FreeAndNil(localVectorFrame);
  FreeAndNil(localCandelFrame);
end;

end.
