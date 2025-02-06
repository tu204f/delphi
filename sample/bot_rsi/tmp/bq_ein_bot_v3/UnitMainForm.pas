unit UnitMainForm;

interface

{$I D:\work\git\delphi\sample\bot_rsi\bq_rsi_2\lb\debug.inc}

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
  FMX.Menus, FMX.ListBox, FMXTee.Engine, FMXTee.Series, FMXTee.Procs,
  FMXTee.Chart;

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
    GridPanelLayout: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    TextManager: TText;
    TextMirrorManager: TText;
    StrGrid_V2: TStringGrid;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    Chart: TChart;
    SeriesValueRSI: TLineSeries;
    SeriesValueMaRSI: TLineSeries;
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure MenuItemSaveFileClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
    procedure _MirrorCloseOrder;
  protected
    procedure DoStart;
    procedure DoStop;
    procedure Strategy;
    procedure PositionClose(ASander: TObject);
    procedure MirrorPositionClose(ASander: TObject);
    function GetQuantity: Double;
    function GetMirrorQuantity: Double;
  protected
    procedure SetLogOp(S: String);
  public
    TypeDirection: TTypeDirection;

    Position: TJournalPosition;
    JournalManager: TJournalManager;

    MirrorPosition: TJournalPosition;
    MirrorJournalManager: TJournalManager;

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
  SetAddColumn(StringGridCandel,'RSI_ATR');
  SetAddColumn(StringGridCandel,'ATR');

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
  SetAddColumn(StrGrid,'ProfitFeeRatesTaker');
  SetAddColumn(StrGrid,'ProfitFeeRatesMaker');

  SetAddColumn(StrGrid_V2,'id',50);
  SetAddColumn(StrGrid_V2,'OpenTime',120);
  SetAddColumn(StrGrid_V2,'OpenPrice');
  SetAddColumn(StrGrid_V2,'CloseTime',120);
  SetAddColumn(StrGrid_V2,'ClosePrice');
  SetAddColumn(StrGrid_V2,'Qty');
  SetAddColumn(StrGrid_V2,'Side');
  SetAddColumn(StrGrid_V2,'SL');
  SetAddColumn(StrGrid_V2,'TK');
  SetAddColumn(StrGrid_V2,'Profit');
  SetAddColumn(StrGrid_V2,'TypeTrade');
  SetAddColumn(StrGrid_V2,'FeeRatesTaker');
  SetAddColumn(StrGrid_V2,'FeeRatesMaker');
  SetAddColumn(StrGrid_V2,'ProfitFeeRatesTaker');
  SetAddColumn(StrGrid_V2,'ProfitFeeRatesMaker');

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;

  TPlatfomBybit(TradingPlatform).ApiKey := '3bvDxJnKzjkIg8y0RV';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_5;

  Position := nil;
  JournalManager := TJournalManager.Create;

  MirrorPosition := nil;
  MirrorJournalManager := TJournalManager.Create;

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(MirrorJournalManager);
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
  end;
end;

procedure TMainForm.DoStop;
begin
  if TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Старт';
    TradingPlatform.Stop;
  end;
end;

procedure TMainForm.ButtonStartOrStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    DoStop
  else
    DoStart;
end;

procedure TMainForm.SetLogOp(S: String);
begin
  {лог операции}
end;

procedure TMainForm.Strategy;
var
  xRSI: Double;
begin
  if not Assigned(Position) then
  begin
    xRSI := TradingPlatform.ValueRSI.RSI;
    if xRSI > 50 then
    begin
      case TypeDirection of
        tdNull: TypeDirection := TTypeDirection.tdLong;
        tdShort: begin
          TypeDirection := TTypeDirection.tdLong;
          ButtonBuyClick(nil);
        end;
      end;
    end else if xRSI < 50 then
    begin
      case TypeDirection of
        tdNull: TypeDirection := TTypeDirection.tdShort;
        tdLong: begin
          TypeDirection := TTypeDirection.tdShort;
          ButtonSellClick(nil);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.PositionClose(ASander: TObject);
begin
  Position := nil;
  // Закрываем зиркальную позицию
  Self._MirrorCloseOrder();
end;

procedure TMainForm.MirrorPositionClose(ASander: TObject);
begin
  MirrorPosition := nil;
end;


procedure TMainForm.TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);

  procedure _ShowCandel;
  var
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    SeriesValueRSI.Clear;
    SeriesValueMaRSI.Clear;

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
        StringGridCandel.Cells[8,i] := TradingPlatform.ValueRSI.ValueATR[i].ToString;
        StringGridCandel.Cells[9,i] := TradingPlatform.ValueATR.Values[i].ToString;

        SeriesValueRSI.Add(TradingPlatform.ValueRSI.ValueRSI[(iCount - 1) - i]);
        SeriesValueMaRSI.Add(TradingPlatform.ValueRSI.ValueMaRSI[(iCount- 1) - i]);

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

        StrGrid.Cells[11,i] := FloatToStr(xPosition.FeeRatesTaker);
        StrGrid.Cells[12,i] := FloatToStr(xPosition.FeeRatesMaker);
        StrGrid.Cells[13,i] := FloatToStr(xPosition.ProfitFeeRatesTaker);
        StrGrid.Cells[14,i] := FloatToStr(xPosition.ProfitFeeRatesMaker);
      end;
  end;

  procedure _ShowMirrorPosition;
  var
    i, Count: Integer;
    xPosition: TJournalPosition;
  begin
    Count := MirrorJournalManager.Positions.Count;
    StrGrid_v2.RowCount := Count;

    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xPosition := MirrorJournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttClose then
          xPosition.SetUpdata;

        StrGrid_v2.Cells[0,i] := (i + 1).ToString;
        StrGrid_v2.Cells[1,i] := DateTimeToStr(xPosition.OpenTime);
        StrGrid_v2.Cells[2,i] := FloatToStr(xPosition.OpenPrice);

        if xPosition.ClosePrice = 0 then
        begin
          StrGrid_v2.Cells[3,i] := '';
          StrGrid_v2.Cells[4,i] := '';
        end else
        begin
          StrGrid_v2.Cells[3,i] := DateTimeToStr(xPosition.CloseTime);
          StrGrid_v2.Cells[4,i] := FloatToStr(xPosition.ClosePrice);
        end;

        StrGrid_v2.Cells[5,i] := FloatToStr(xPosition.Qty);
        StrGrid_v2.Cells[6,i] := GetStrToSide(xPosition.Side);
        StrGrid_v2.Cells[7,i] := FloatToStr(xPosition.StopLoss);
        StrGrid_v2.Cells[8,i] := FloatToStr(xPosition.TakeProfit);
        StrGrid_v2.Cells[9,i] := FloatToStr(xPosition.Profit);
        StrGrid_v2.Cells[10,i] := GetStrToTypeTrade(xPosition.TypeTrade);

        StrGrid_v2.Cells[11,i] := FloatToStr(xPosition.FeeRatesTaker);
        StrGrid_v2.Cells[12,i] := FloatToStr(xPosition.FeeRatesMaker);
        StrGrid_v2.Cells[13,i] := FloatToStr(xPosition.ProfitFeeRatesTaker);
        StrGrid_v2.Cells[14,i] := FloatToStr(xPosition.ProfitFeeRatesMaker);
      end;
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
  if Assigned(Position) then
  begin
    case Position.Side of
      TTypeBuySell.tsBuy: Position.SetUpData(TradingPlatform.StateMarket.Bid);
      TTypeBuySell.tsSell: Position.SetUpData(TradingPlatform.StateMarket.Ask);
    end;
  end;
  TextManager.Text := 'Прямое направление торговли: Profit := ' +
    JournalManager.Profit.ToString + '//' +
    JournalManager.ProfitFeeRatesTaker.ToString + '//' +
    JournalManager.ProfitFeeRatesMaker.ToString + ';';


  if Assigned(MirrorPosition) then
  begin
    case MirrorPosition.Side of
      TTypeBuySell.tsBuy: MirrorPosition.SetUpData(TradingPlatform.StateMarket.Bid);
      TTypeBuySell.tsSell: MirrorPosition.SetUpData(TradingPlatform.StateMarket.Ask);
    end;
  end;
  TextMirrorManager.Text := 'Зеркальное направление торговли: Profit := ' +
    MirrorJournalManager.Profit.ToString + '//' +
    MirrorJournalManager.ProfitFeeRatesTaker.ToString + '//' +
    MirrorJournalManager.ProfitFeeRatesMaker.ToString + ';';

  _ShowPosition;
  _ShowMirrorPosition;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);

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

    xStr.SaveToFile('mirror_positions.csv');
  finally
    FreeAndNil(xStr);
  end;
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


function TMainForm.GetQuantity: Double;
//var
//  Count: Integer;
//  xPosition: TJournalPosition;
begin
// Вслучае получение отрицательного профита прошлый раз значение удваевыем
  Result := 1;
//  Count := JournalManager.Positions.Count;
//  if Count > 0 then
//  begin
//    xPosition := JournalManager.Positions[Count - 1];
//    if xPosition.Profit <= 0 then
//      Result := 2 * xPosition.Qty;
//  end;
end;

function TMainForm.GetMirrorQuantity: Double;
var
  Count: Integer;
  xPosition: TJournalPosition;
begin
  // Вслучае получение отрицательного профита прошлый раз значение удваевыем
  Result := 1;
  Count := MirrorJournalManager.Positions.Count;
  if Count > 0 then
  begin
    xPosition := MirrorJournalManager.Positions[Count - 1];
    if xPosition.Profit <= 0 then
      Result := 2 * xPosition.Qty;
  end;
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);

  procedure _Order();
  var
    xQuantity: Double;
  begin
    xQuantity := GetQuantity;
    if not Assigned(Position) then
    begin
      Position := JournalManager.GetCreateJournalPosition;
      Position.OnClose := PositionClose;
      with Position do
      begin
        OpenTime := GetNewDateTime;
        OpenPrice := TradingPlatform.StateMarket.Ask;
        Qty := xQuantity;
        Side := TTypeBuySell.tsBuy;
        IsActive := True;
        TypeTrade := TTypeTrade.ttOpen;
        Triling := 15;
        TakeProfit := OpenPrice + 10;
        DoOpen;
      end;
    end;
  end;

  procedure _MirrorOrder();
  var
    xQuantity: Double;
  begin
    xQuantity := GetMirrorQuantity;
    if not Assigned(MirrorPosition) then
    begin
      MirrorPosition := MirrorJournalManager.GetCreateJournalPosition;
      MirrorPosition.OnClose := MirrorPositionClose;
      with MirrorPosition do
      begin
        OpenTime := GetNewDateTime;
        OpenPrice := TradingPlatform.StateMarket.Bid;
        Qty := xQuantity;
        Side := TTypeBuySell.tsSell;
        IsActive := True;
        TypeTrade := TTypeTrade.ttOpen;
        Triling := 20;
        TakeProfit := OpenPrice - 10;
        DoOpen;
      end;
    end;
  end;

begin
  _Order();
  _MirrorOrder();
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);

  procedure _Order();
  var
    xQuantity: Double;
  begin
    xQuantity := GetQuantity;
    if not Assigned(Position) then
    begin
      Position := JournalManager.GetCreateJournalPosition;
      Position.OnClose := PositionClose;
      with Position do
      begin
        OpenTime := GetNewDateTime;
        OpenPrice := TradingPlatform.StateMarket.Bid;
        Qty := xQuantity;
        Side := TTypeBuySell.tsSell;
        IsActive := True;
        TypeTrade := TTypeTrade.ttOpen;
        Triling := 15;
        TakeProfit := OpenPrice - 10;
        DoOpen;
      end;
    end;
  end;

  procedure _MirrorOrder();
  var
    xQuantity: Double;
  begin
    xQuantity := GetMirrorQuantity;
    if not Assigned(MirrorPosition) then
    begin
      MirrorPosition := MirrorJournalManager.GetCreateJournalPosition;
      MirrorPosition.OnClose := MirrorPositionClose;
      with MirrorPosition do
      begin
        OpenTime := GetNewDateTime;
        OpenPrice := TradingPlatform.StateMarket.Ask;
        Qty := xQuantity;
        Side := TTypeBuySell.tsBuy;
        IsActive := True;
        TypeTrade := TTypeTrade.ttOpen;
        Triling := 20;
        TakeProfit := OpenPrice + 10;
        DoOpen;
      end;
    end;
  end;

begin
  _Order();
  _MirrorOrder();
end;


(* ************************************************************************** *)
(* Закрытие позицию                                                           *)
(* ************************************************************************** *)

procedure TMainForm._MirrorCloseOrder();
begin
  if Assigned(MirrorPosition) then
  begin
    with MirrorPosition do
    begin
      CloseTime := GetNewDateTime;
      case Side of
        TTypeBuySell.tsBuy: ClosePrice := TradingPlatform.StateMarket.Bid;
        TTypeBuySell.tsSell: ClosePrice := TradingPlatform.StateMarket.Ask;
      end;
      IsActive := False;
      TypeTrade := TTypeTrade.ttClose;
      DoClose;
    end;
  end;
end;

procedure TMainForm.ButtonCloseClick(Sender: TObject);

  procedure _CloseOrder();
  begin
    if Assigned(Position) then
    begin
      with Position do
      begin
        CloseTime := GetNewDateTime;
        case Side of
          TTypeBuySell.tsBuy: ClosePrice := TradingPlatform.StateMarket.Bid;
          TTypeBuySell.tsSell: ClosePrice := TradingPlatform.StateMarket.Ask;
        end;
        IsActive := False;
        TypeTrade := TTypeTrade.ttClose;
        DoClose;
      end;
    end;
  end;

begin
  // Закрыть позицию
  _CloseOrder();
  _MirrorCloseOrder();
end;


end.
