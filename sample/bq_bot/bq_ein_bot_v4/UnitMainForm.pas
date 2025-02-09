unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,

  Lb.Indiсator,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Platform.Bybit,
  FMX.Objects,
  FMX.Layouts,
  Lb.Journal.Trading.v2,
  FMX.TabControl,
  FMX.Menus,
  FMX.Edit,
  Lb.Bybit.ServerTime;

type
  TMainForm = class(TForm)
    ButtonStartOrStop: TButton;
    Rectangle: TRectangle;
    TextStatus: TText;
    Layout: TLayout;
    GridLayout: TGridPanelLayout;
    ButtonBuy: TButton;
    ButtonSell: TButton;
    ButtonClose: TButton;
    TabControl1: TTabControl;
    TabItemTrade: TTabItem;
    TabItemPosition: TTabItem;
    StrGrid: TStringGrid;
    StringGridCandel: TStringGrid;
    PopupMenu: TPopupMenu;
    MenuItemSaveFile: TMenuItem;
    Timer: TTimer;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure MenuItemSaveFileClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
  protected
    procedure DoStart;
    procedure DoStop;
    procedure Strategy;
    procedure PositionClose(ASander: TObject);
  public
    TypeDirection: TTypeDirection;
    JournalManager: TJournalManager;
    TradingPlatform: TTradingPlatform;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.DateUtils;

constructor TMainForm.Create(AOwner: TComponent);

  procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := AStrGrid;
    xCol.Header := AHeader;
    xCol.Width  := AWidth;
  end;

begin
  inherited;

  SetAddColumn(StringGridCandel,'Time',150);
  SetAddColumn(StringGridCandel,'Open');
  SetAddColumn(StringGridCandel,'High');
  SetAddColumn(StringGridCandel,'Low');
  SetAddColumn(StringGridCandel,'Close');
  SetAddColumn(StringGridCandel,'Vol');
  SetAddColumn(StringGridCandel,'RSI');
  SetAddColumn(StringGridCandel,'MaRSI');
  SetAddColumn(StringGridCandel,'ATR');
  SetAddColumn(StringGridCandel,'Momentum');
  SetAddColumn(StringGridCandel,'MomentumMA');


  SetAddColumn(StrGrid,'id',50);
  SetAddColumn(StrGrid,'OpenTime',120);
  SetAddColumn(StrGrid,'OpenPrice');
  SetAddColumn(StrGrid,'CloseTime',120);
  SetAddColumn(StrGrid,'ClosePrice');
  SetAddColumn(StrGrid,'Qty');
  SetAddColumn(StrGrid,'Side');
  SetAddColumn(StrGrid,'SL');
  SetAddColumn(StrGrid,'TK');
  SetAddColumn(StrGrid,'Profit');
  SetAddColumn(StrGrid,'TypeTrade');
  SetAddColumn(StrGrid,'FeeRatesTaker');
  SetAddColumn(StrGrid,'FeeRatesMaker');

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;

  TPlatfomBybit(TradingPlatform).ApiKey := '3bvDxJnKzjkIg8y0RV';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_15;

  JournalManager := TJournalManager.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(JournalManager);
  FreeAndNil(TradingPlatform);
  inherited;
end;

procedure TMainForm.DoStart;
begin
  if not TradingPlatform.IsActive then
  begin
    TypeDirection := TTypeDirection.tdNull;

    ButtonStartOrStop.Text := 'Стоп';
    TradingPlatform.Symbol := 'ETHUSDT';
    TradingPlatform.StateMarket.Qty := 1;
    TradingPlatform.Start;

    Timer.Enabled := True;
  end;
end;

procedure TMainForm.DoStop;
begin
  if TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Старт';
    TradingPlatform.Stop;

    Timer.Enabled := False;
  end;
end;


procedure TMainForm.ButtonStartOrStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    DoStop
  else
    DoStart;
end;

procedure TMainForm.Strategy;
begin
  //
end;

procedure TMainForm.PositionClose(ASander: TObject);
begin
  //
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if TradingPlatform.ValueMomentum.MomentumMA > 0 then
    ButtonBuyClick(nil)
  else if TradingPlatform.ValueMomentum.MomentumMA < 0 then
    ButtonSellClick(nil);
end;

procedure TMainForm.TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);

  procedure _ShowCandel;
  var
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    iCount := AStateMarket.Candels.Count;
    StringGridCandel.RowCount := iCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := AStateMarket.Candels[i];
        StringGridCandel.Cells[0,i] := DateTimeToStr(UnixToDateTime(xCandel.Time));
        StringGridCandel.Cells[1,i] := xCandel.Open.ToString;
        StringGridCandel.Cells[2,i] := xCandel.High.ToString;
        StringGridCandel.Cells[3,i] := xCandel.Low.ToString;
        StringGridCandel.Cells[4,i] := xCandel.Close.ToString;
        StringGridCandel.Cells[5,i] := xCandel.Vol.ToString;
        StringGridCandel.Cells[6,i] := TradingPlatform.ValueRSI.ValueRSI[i].ToString;
        StringGridCandel.Cells[7,i] := TradingPlatform.ValueRSI.ValueMaRSI[i].ToString;
        StringGridCandel.Cells[8,i] := TradingPlatform.ValueATR.Values[i].ToString;
        StringGridCandel.Cells[9,i]  := TradingPlatform.ValueMomentum.Values[i].ToString;
        StringGridCandel.Cells[10,i] := TradingPlatform.ValueMomentum.ValuesMA[i].ToString;
      end;
  end;

  procedure _ShowPosition;
  var
    i, Count: Integer;
    xPosition: TJournalPosition;
  begin
    Count := JournalManager.Positions.Count;
    StrGrid.RowCount := Count;

    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xPosition := JournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttClose then
          xPosition.SetUpdata;

        StrGrid.Cells[0,i] := (i + 1).ToString;
        StrGrid.Cells[1,i] := DateTimeToStr(xPosition.OpenTime);
        StrGrid.Cells[2,i] := FloatToStr(xPosition.OpenPrice);

        if xPosition.ClosePrice = 0 then
        begin
          StrGrid.Cells[3,i] := '';
          StrGrid.Cells[4,i] := '';
        end else
        begin
          StrGrid.Cells[3,i] := DateTimeToStr(xPosition.CloseTime);
          StrGrid.Cells[4,i] := FloatToStr(xPosition.ClosePrice);
        end;

        StrGrid.Cells[5,i] := FloatToStr(xPosition.Qty);
        StrGrid.Cells[6,i] := GetStrToSide(xPosition.Side);
        StrGrid.Cells[7,i] := FloatToStr(xPosition.StopLoss);
        StrGrid.Cells[8,i] := FloatToStr(xPosition.TakeProfit);
        StrGrid.Cells[9,i] := FloatToStr(xPosition.Profit);
        StrGrid.Cells[10,i] := GetStrToTypeTrade(xPosition.TypeTrade);
        StrGrid.Cells[11,i] := FloatToStr(xPosition.ProfitFeeRatesTaker);
        StrGrid.Cells[12,i] := FloatToStr(xPosition.ProfitFeeRatesMaker);
      end;
  end;

  procedure _ShowUpDataPosition;
  var
    xPrice: Double;
    i, Count: Integer;
    xPosition: TJournalPosition;
  var
    xSumProfit, xSumTaker, xSumMaker: Double;
  begin
    xSumProfit := 0;
    xSumTaker := 0;
    xSumMaker := 0;
    Count := JournalManager.Positions.Count;
    if Count > 0 then
    begin
      for i := 0 to Count - 1 do
      begin
        xPosition := JournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttOpen then
        begin
          case xPosition.Side of
            TTypeBuySell.tsBuy : xPrice := TradingPlatform.StateMarket.Ask;
            TTypeBuySell.tsSell: xPrice := TradingPlatform.StateMarket.Bid;
          else
            xPrice := 0;
          end;
          if xPrice > 0 then
            xPosition.SetUpdata(xPrice);
        end;
        xSumProfit := xSumProfit + xPosition.Profit;
        xSumTaker  := xSumTaker + xPosition.ProfitFeeRatesTaker;
        xSumMaker  := xSumMaker + xPosition.ProfitFeeRatesMaker;
      end;
    end;
    Edit1.Text := xSumProfit.ToString;
    Edit2.Text := xSumTaker.ToString;
    Edit3.Text := xSumMaker.ToString;
  end;

begin
  // ***********************************************
  // Оценка состояния рынка
  TextStatus.Text :=
    'Price: ' + TradingPlatform.StateMarket.Ask.ToString + '/' + TradingPlatform.StateMarket.Bid.ToString + '; ' +
    'ValueRSI: ' + TradingPlatform.ValueRSI.RSI.ToString + '; ' +
    'ValueAveragRSI: ' + TradingPlatform.ValueRSI.MovingAveragRSI.ToString + '; ' +
    'ValueATR: ' + TradingPlatform.ValueATR.ATR.ToString  + ';';

  _ShowCandel;
  Strategy;

  _ShowUpDataPosition;

  _ShowPosition;
end;

procedure TMainForm.MenuItemSaveFileClick(Sender: TObject);

  function _Add(S: String): String;
  begin
    Result := S + ';';
  end;

var
  xS: String;
  xStr: TStrings;
  i, Count: Integer;
  xPosition: TJournalPosition;
begin
  xStr := TStringList.Create;
  try
    Count := JournalManager.Positions.Count;
    StrGrid.RowCount := Count;

    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xPosition := JournalManager.Positions[i];

        xS := _Add((i + 1).ToString);
        xS := _Add(DateTimeToStr(xPosition.OpenTime));
        xS := _Add(FloatToStr(xPosition.OpenPrice));

        if xPosition.ClosePrice = 0 then
        begin
          xS := _Add('');
          xS := _Add('');
        end else
        begin
          xS := _Add(DateTimeToStr(xPosition.CloseTime));
          xS := _Add(FloatToStr(xPosition.ClosePrice));
        end;

        xS := _Add(FloatToStr(xPosition.Qty));
        xS := _Add(GetStrToSide(xPosition.Side));
        xS := _Add(FloatToStr(xPosition.StopLoss));
        xS := _Add(FloatToStr(xPosition.TakeProfit));
        xS := _Add(FloatToStr(xPosition.Profit));
        xS := _Add(GetStrToTypeTrade(xPosition.TypeTrade));

        xStr.Add(xS);
      end;

    xStr.SaveToFile('positions.csv');
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
var
  xPrice: Double;
  xPosition: TJournalPosition;
begin
  xPrice := TradingPlatform.StateMarket.Ask;
  if xPrice > 0 then
  begin
    xPosition := JournalManager.GetCreateJournalPosition;
    xPosition.OnClose := PositionClose;
    with xPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := xPrice;
      Qty := 1;
      Side := TTypeBuySell.tsBuy;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Triling := 15;
      DoOpen;
    end;
  end;
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);
var
  xPrice: Double;
  xPosition: TJournalPosition;
begin
  xPrice := TradingPlatform.StateMarket.Bid;
  if xPrice > 0 then
  begin
    xPosition := JournalManager.GetCreateJournalPosition;
    xPosition.OnClose := PositionClose;
    with xPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := xPrice;
      Qty := 1;
      Side := TTypeBuySell.tsSell;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Triling := 15;
      DoOpen;
    end;
  end;
end;

procedure TMainForm.ButtonCloseClick(Sender: TObject);
var
  xPrice: Double;
  xPosition: TJournalPosition;
  i, iCount: Integer;
begin
  iCount := JournalManager.Positions.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
    begin
      xPosition := JournalManager.Positions[i];
      with xPosition do
      begin
        if TypeTrade = TTypeTrade.ttClose then
          Exit;

        case Side of
          TTypeBuySell.tsBuy: xPrice := TradingPlatform.StateMarket.Bid;
          TTypeBuySell.tsSell: xPrice := TradingPlatform.StateMarket.Ask;
        else
          xPrice := 0;
        end;

        if xPrice <= 0 then
          Exit;

        CloseTime := GetNewDateTime;
        ClosePrice := xPrice;
        IsActive := False;
        TypeTrade := TTypeTrade.ttClose;
        DoClose;
      end;
    end;
end;


end.
