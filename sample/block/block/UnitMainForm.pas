unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.StdCtrls,
  UnitChartCandelsFrame,
  Lb.SysUtils.Candel,
  Lb.Block, FMX.Objects;

type
  TMainForm = class(TForm)
    TabControl: TTabControl;
    LayoutBootom: TLayout;
    ButtonNext: TButton;
    TabItemChart: TTabItem;
    Layout: TLayout;
    ButtonCreateBlock: TButton;
    Text1: TText;
    Timer: TTimer;
    ButtonStart: TButton;
    procedure ButtonNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCreateBlockClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IndexBlock: Integer;
    Blocks: TBlockList;
    ChartCandelsFrame: TChartCandelsFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.ButtonCreateBlockClick(Sender: TObject);
const
  SIZE_BLOCK = 65;
var
  xFN: String;
  xS: String;
  xBlockStr: TStrings;
  xSource: TStrings;
  xBlock: TBlock;
begin
  xFN := 'd:\work\git\delphi\sample\block\bin\data\SPFB.Si_230101_230918.csv';
  xSource   := TStringList.Create;
  xBlockStr := TStringList.Create;
  try
    xSource.LoadFromFile(xFN);
    for var i := 0 to xSource.Count - SIZE_BLOCK do
    begin
      xBlockStr.Clear;
      for var j := 0 to SIZE_BLOCK - 1 do
      begin
        xS := xSource[i + j];
        xBlockStr.Add(xS);
      end;
      xBlock := TBlock.Create;
      xBlock.SetParserBlock(xBlockStr);
      Blocks.Add(xBlock);
    end;
  finally
    FreeAndNil(xBlockStr);
    FreeAndNil(xSource);
  end;
  ShowMessage('Разбиты на блоки');
end;



procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(Blocks);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  IndexBlock := 0;
  Blocks := TBlockList.Create;

  ChartCandelsFrame := TChartCandelsFrame.Create(Self);
  ChartCandelsFrame.Parent := Layout;
  ChartCandelsFrame.Align := TAlignLayout.Client;

end;


procedure TMainForm.ButtonNextClick(Sender: TObject);
var
  xBlock: TBlock;
begin

  Inc(IndexBlock);


  xBlock := Blocks[IndexBlock];

  ChartCandelsFrame.Candels.Copy(xBlock.Candels);
  ChartCandelsFrame.BuildChart;

  Text1.Text := 'Количество свячей:' + xBlock.Candels.Count.ToString;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  Timer.Enabled := not Timer.Enabled;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xBlock: TBlock;
begin
  try

    Inc(IndexBlock);


    xBlock := Blocks[IndexBlock];

    ChartCandelsFrame.Candels.Copy(xBlock.Candels);
    ChartCandelsFrame.BuildChart;

    Text1.Text :=
      'Количество блоков: ' + Blocks.Count.ToString + ' ' +
      'Количество свячей:' + xBlock.Candels.Count.ToString;

    if IndexBlock >= Blocks.Count then
      Timer.Enabled := False;

  except
    Timer.Enabled := False;
  end;
end;

end.
