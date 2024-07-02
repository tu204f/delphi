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
    TimerCandel: TTimer;
    lbLog: TListBox;
    Button1: TButton;
    procedure TimerCandelTimer(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    procedure SetLog(S: String);
  public
    CandelIndex: Integer;
    CandelsSource: TCandelsSource;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLoadCandelsSource;
  public
    procedure localTraderOnEventStart(Sander: TObject);
    procedure localTraderOnEventStop(Sander: TObject);
    procedure localTraderOnEventProgress(Sander: TObject; AProgress: Integer);
  public
    Trader: TWorkTrader;
    procedure SetÑriterion(const AOpenRSI, ACloseRSI: Integer);
    procedure SetLoadTrader;
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

var
  localTrader: TWorkTraderThread = nil;


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
  CandelsSource := TCandelsSource.Create;
  Trader := TWorkTrader.Create;
  Trader.CreateÑriterion(1);
  Trader.Side := tsBuy;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Trader);
  FreeAndNil(CandelsSource);
  inherited;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  lbLog.Items.Clear;
  TimerCandel.Enabled := False;
end;


procedure TMainForm.SetLog(S: String);
var
  xS: String;
begin
  xS := FormatDateTime('hh:mm:ss',Time) + ' || ' + S;
  lbLog.Items.Add(xS);
end;

procedure TMainForm.SetLoadCandelsSource;
var
  xFileName: String;
begin
  // Çàãðóæàåì ìàññèâ ñâå÷åé
  xFileName := ExtractFilePath(ParamStr(0)) + 'data\';
  xFileName := xFileName + 'SBER_240624_240702.csv';// 'SPFB.SBRF_240301_240627.csv';
  CandelsSource.LoadFromFile(xFileName);
end;

procedure TMainForm.localTraderOnEventStart(Sander: TObject);
begin
  SetLog('Ñòàðò');
end;

procedure TMainForm.localTraderOnEventProgress(Sander: TObject; AProgress: Integer);
begin
  SetLog(' >> ' + AProgress.ToString);
end;

procedure TMainForm.localTraderOnEventStop(Sander: TObject);
begin
  SetLog('Ñòîï');
  localTrader := nil;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0)) + 'data\';
  xFileName := xFileName + 'SPFB.SBRF_240301_240627.csv';

  localTrader := TWorkTraderThread.Create;
  localTrader.FileName := xFileName;
  localTrader.PeriodRSI := 14;
  localTrader.MinusProfit := -1000;

  localTrader.OnEventStart := localTraderOnEventStart;
  localTrader.OnEventStop := localTraderOnEventStop;
  localTrader.OnEventProgress := localTraderOnEventProgress;

  localTrader.Start;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  CandelIndex := 1;

  // Ñ íà÷àëî çàãðóæàåì ìàññèâ ñâå÷åé
  SetLoadCandelsSource;
  SetLog('Êîëè÷åñòâî ñâå÷åé: ' + CandelsSource.Count.ToString);

  SetÑriterion(30,50);

  TimerCandel.Enabled := True;
end;

procedure TMainForm.SetÑriterion(const AOpenRSI, ACloseRSI: Integer);
var
  xÑriterion: TÑriterion;
begin
  Trader.Side := TTypeSide.tsBuy;
  Trader.CreateÑriterion(1);
  // ---------------------------------
  xÑriterion := Trader.Ñriterions[0];
  xÑriterion.RSI := AOpenRSI;
  xÑriterion.ReActiveRSI := xÑriterion.RSI + 10;
  xÑriterion.Qty := 1;
  xÑriterion.IsActive := True;
  // ---------------------------------
  xÑriterion := Trader.Ñriterions[1];
  xÑriterion.RSI := ACloseRSI;
  xÑriterion.ReActiveRSI := xÑriterion.RSI - 10;
  xÑriterion.Qty := 1;
  xÑriterion.IsActive := True;
end;

procedure TMainForm.SetLoadTrader;

  procedure _TradeGrid(ASource: TStrings; APosition: TPositionTrade);
  var
    xS: String;
    i, iCount: Integer;
    xTrade: TTrade;
  begin
    iCount := APosition.Trades.Count;
    if iCount > 0 then
    begin

      for i := 0 to iCount - 1 do
      begin
        xTrade := APosition.Trades[i];
        xS := ';;' + DateToStr(xTrade.Date) + ';';
        xS := xS + TimeToStr(xTrade.Time) + ';';
        xS := xS + xTrade.Price.ToString + ';';
        xS := xS + xTrade.Qty.ToString + ';';
        xS := xS + GetTypeSideToStr(xTrade.Side) + ';';
        ASource.Add(xS);
      end;

      xTrade := APosition.CloseTrade;
      xS := ';;' + DateToStr(xTrade.Date) + ';';
      xS := xS + TimeToStr(xTrade.Time) + ';';
      xS := xS + xTrade.Price.ToString + ';';
      xS := xS + xTrade.Qty.ToString + ';';
      xS := xS + GetTypeSideToStr(xTrade.Side) + ';';
      ASource.Add(xS);
    end;
  end;

  procedure _PositionGrid(ASource: TStrings);
  var
    xS: String;
    i, iCount: Integer;
    xPosition: TPositionTrade;
  begin
    iCount := Trader.PositionTrades.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xPosition := Trader.PositionTrades[i];
        xS := xPosition.Price.ToString + ';';
        xS := xS + xPosition.Qty.ToString + ';';
        xS := xS + GetTypeSideToStr(xPosition.Side) + ';';
        if xPosition.TypePosition = tpClose then
        begin
          xS := xS + xPosition.CloseTrade.Price.ToString + ';';
          xS := xS + xPosition.CloseTrade.Qty.ToString + ';';
          xS := xS + xPosition.Profit.ToString + ';';
        end;

        ASource.Add(xS);
        _TradeGrid(ASource, xPosition);
      end;
    end;
  end;

var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    _PositionGrid(xStr);
    xStr.SaveToFile('result.csv');
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TMainForm.TimerCandelTimer(Sender: TObject);
var
  xValRSI: Double;
  xCandel: TCandel;
  xCandels: TCandelList;
begin
  try
    if CandelsSource.Count > CandelIndex then
    begin
      xCandels := TCandelList.Create;
      try
        SetCandels(CandelIndex, 14, CandelsSource, xCandels);
        if xCandels.Count = 14 then
        begin
          xCandel := xCandels[13];
          xValRSI := GetRSI(xCandels);
          Trader.SetUpDateCandel(xCandel,xValRSI);
          SetLog('price: ' + xCandel.Close.ToString + '; ValRSI: ' + xValRSI.ToString);
        end;
      finally
        FreeAndNil(xCandels);
      end;
      Inc(CandelIndex);
    end
    else
    begin
      TimerCandel.Enabled := False;
      SetLoadTrader;
      SetLog('Ñòîï');
    end;
  except
    TimerCandel.Enabled := False;
    SetLoadTrader;
    SetLog('Ñòîï');
  end;
end;

end.
