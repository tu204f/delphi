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
  Lb.Journal.Trading.v2, FMX.TabControl;

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
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
  protected
    procedure DoStart;
    procedure DoStop;
  public
    Position: TJournalPosition;
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

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;

  TPlatfomBybit(TradingPlatform).ApiKey := '3bvDxJnKzjkIg8y0RV';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_5;

  Position := nil;
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

  if Assigned(Position) then
  begin
    case Position.Side of
      TTypeBuySell.tsBuy: Position.SetUpData(TradingPlatform.StateMarket.Bid);
      TTypeBuySell.tsSell: Position.SetUpData(TradingPlatform.StateMarket.Ask);
    end;
  end;
  _ShowPosition;
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
begin
  // Купить
  if not Assigned(Position) then
  begin
    Position := JournalManager.GetCreateJournalPosition;
    with Position do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := TradingPlatform.StateMarket.Ask;
      Qty := 1;
      Side := TTypeBuySell.tsBuy;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Triling := 15;
    end;
  end;
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);
begin
  // Продать
  if not Assigned(Position) then
  begin
    Position := JournalManager.GetCreateJournalPosition;
    with Position do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := TradingPlatform.StateMarket.Bid;
      Qty := 1;
      Side := TTypeBuySell.tsSell;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Triling := 15;
    end;
  end;
end;

procedure TMainForm.ButtonCloseClick(Sender: TObject);
begin
  // Закрыть позицию
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
    end;
    Position := nil;
  end;
end;


end.
