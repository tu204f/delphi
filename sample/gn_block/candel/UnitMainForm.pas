unit UnitMainForm;

interface

{$IFDEF DEBUG}
//  {$DEFINE DB_LOG}
{$ENDIF}

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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ListBox,

  Lb.ReadPrice,
  Lb.SysUtils,

  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid, FMX.Objects;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    StringGrid: TStringGrid;
    Text1: TText;
    procedure ButtonStartClick(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public {Событие потока}
    procedure localTraderOnEventStart(Sander: TObject);
    procedure localTraderOnEventStop(Sander: TObject);
    procedure localTraderOnEventProgress(Sander: TObject; AProgress: Integer);
  end;

var
  MainForm: TMainForm;

{$IFDEF DB_LOG}
procedure Log(S: String);
{$ENDIF}

implementation

{$R *.fmx}

uses
  Lb.WorkTraderThread;

{$IFDEF DB_LOG}
procedure Log(S: String);
begin
  MainForm.SetLog(S);
end;
{$ENDIF}

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddColumn(const AStringGrid: TStringGrid; const AName: String);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(nil);
    xColumn.Header := AName;
    xColumn.Parent := AStringGrid;
  end;

begin
  inherited;
  _AddColumn(StringGrid,'ID');
  _AddColumn(StringGrid,'Side');
  _AddColumn(StringGrid,'Open.RSI');
  _AddColumn(StringGrid,'Open.Active.RSI');
  _AddColumn(StringGrid,'Close.RSI');
  _AddColumn(StringGrid,'Close.Active.RSI');
  _AddColumn(StringGrid,'Profit');
  _AddColumn(StringGrid,'Progress');
  _AddColumn(StringGrid,'Position');
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.localTraderOnEventStart(Sander: TObject);
var
  xTrader: TWorkTraderThread;
begin
  xTrader := TWorkTraderThread(Sander);
  StringGrid.Cells[0,xTrader.ID] := xTrader.ID.ToString;
  StringGrid.Cells[1,xTrader.ID] := '';
  StringGrid.Cells[2,xTrader.ID] := '';
  StringGrid.Cells[3,xTrader.ID] := '';
  StringGrid.Cells[4,xTrader.ID] := '';
  StringGrid.Cells[5,xTrader.ID] := '';
  StringGrid.Cells[6,xTrader.ID] := '';
  StringGrid.Cells[7,xTrader.ID] := 'Start';
  StringGrid.Cells[8,xTrader.ID] := '';
end;

procedure TMainForm.localTraderOnEventProgress(Sander: TObject; AProgress: Integer);
var
  xTrader: TWorkTraderThread;
begin
  xTrader := TWorkTraderThread(Sander);
  StringGrid.Cells[0,xTrader.ID] := xTrader.ID.ToString;
  StringGrid.Cells[7,xTrader.ID] := AProgress.ToString;
end;

procedure TMainForm.localTraderOnEventStop(Sander: TObject);
var
  xTrader: TWorkTraderThread;
begin
  xTrader := TWorkTraderThread(Sander);

  StringGrid.Cells[0,xTrader.ID] := xTrader.ID.ToString;
  StringGrid.Cells[1,xTrader.ID] := GetTypeSideToStr(xTrader.Side);
  StringGrid.Cells[2,xTrader.ID] := xTrader.OpenRSI.ToString;
  StringGrid.Cells[3,xTrader.ID] := xTrader.OpenActiveRSI.ToString;
  StringGrid.Cells[4,xTrader.ID] := xTrader.CloseRSI.ToString;
  StringGrid.Cells[5,xTrader.ID] := xTrader.CloseActiveRSI.ToString;
  StringGrid.Cells[6,xTrader.ID] := xTrader.Profit.ToString;
  StringGrid.Cells[7,xTrader.ID] := 'Stop';
  StringGrid.Cells[8,xTrader.ID] :=
    xTrader.Trader.PositivCountProfit.ToString + '/' +
    xTrader.Trader.PositionTrades.Count.ToString;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
var
  xFileName: String;
  xTrader: TWorkTraderThread;
  i: Integer;
begin
  for i := 0 to 9 do
  begin

    xFileName := ExtractFilePath(ParamStr(0)) + 'data\';
    xFileName := xFileName + 'SPFB.SBRF_240301_240627.csv';


    xTrader := TWorkTraderThread.Create;
    xTrader.ID := i;
    xTrader.FileName := xFileName;
    xTrader.Side := TTypeSide.tsBuy;
    xTrader.PeriodRSI := 14;
    xTrader.MinusProfit := -1000;
    xTrader.OnEventStart := localTraderOnEventStart;
    xTrader.OnEventStop := localTraderOnEventStop;
    xTrader.OnEventProgress := localTraderOnEventProgress;
    xTrader.Start;

  end;
end;

end.
