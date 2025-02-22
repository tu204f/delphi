unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,

  Lb.Indicator,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Platform.Bybit,
  Lb.Journal.Trading.v2,
  Lb.Bot,

  FMX.Objects,
  FMX.Layouts,
  FMX.TabControl,
  FMX.Menus,
  FMX.Edit;

type
  TMainForm = class(TForm)
    ButtonStartOrStop: TButton;
    Rectangle: TRectangle;
    TextStatus: TText;
    Layout: TLayout;
    GridLayout: TGridPanelLayout;
    ButtonStartSell: TButton;
    ButtonClose: TButton;
    TabControl1: TTabControl;
    TabItemTrade: TTabItem;
    TabItemPosition: TTabItem;
    StrGrid: TStringGrid;
    StringGridCandel: TStringGrid;
    PopupMenu: TPopupMenu;
    MenuItemSaveFile: TMenuItem;
    ButtonStartBot: TButton;
    BotGrid: TStringGrid;
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure ButtonStartBotClick(Sender: TObject);
    procedure ButtonStartSellClick(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
  protected
    procedure DoStart;
    procedure DoStop;
  public
    LevelBots: TLevelBotList;
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

  SetAddColumn(BotGrid,'id',50);
  SetAddColumn(BotGrid,'Profit');
  SetAddColumn(BotGrid,'Trade.Profit');
  SetAddColumn(BotGrid,'Triling');
  SetAddColumn(BotGrid,'Count');

  SetAddColumn(BotGrid,'RatesTaker');
  SetAddColumn(BotGrid,'RatesMaker');


  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;

  TPlatfomBybit(TradingPlatform).ApiKey := '3bvDxJnKzjkIg8y0RV';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_15;

  LevelBots := TLevelBotList.Create;

end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(LevelBots);
  FreeAndNil(TradingPlatform);
  inherited;
end;

procedure TMainForm.DoStart;

  procedure _SetCreateLevelBot;
  var
    xBot: TLevelBot;
    i: Integer;
  begin
    for i := 1 to 100 do
    begin
      xBot := TLevelBot.Create;
      xBot.Profit := 100;
      xBot.Triling := i;
      xBot.Qty := 1;
      LevelBots.Add(xBot);
    end;
  end;

begin
  if not TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Стоп';
    TradingPlatform.Symbol := 'ETHUSDT';
    TradingPlatform.StateMarket.Qty := 1;
    TradingPlatform.Start;
    _SetCreateLevelBot;
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

procedure TMainForm.ButtonStartBotClick(Sender: TObject);
begin
  for var xBot in LevelBots do
    xBot.Start;
end;


procedure TMainForm.ButtonStartSellClick(Sender: TObject);
begin
  for var xBot in LevelBots do
    xBot.Stop;
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
    xRow: Integer;
    i, Count: Integer;
    xPosition: TJournalPosition;
    xJournalManager: TJournalManager;
  begin
    xRow := BotGrid.Selected;
    if (xRow >= 0) and (xRow < LevelBots.Count) then
    begin

      xJournalManager := LevelBots[xRow].Manager;
      Count := xJournalManager.Positions.Count;
      StrGrid.RowCount := Count;

      if Count > 0 then
        for i := 0 to Count - 1 do
        begin
          xPosition := xJournalManager.Positions[i];
          if xPosition.TypeTrade = TTypeTrade.ttClose then
            xPosition.SetUpdata
          else
            if Assigned(xPosition) then
            begin
              case xPosition.Side of
                TTypeBuySell.tsBuy: xPosition.SetUpData(TradingPlatform.StateMarket.Bid);
                TTypeBuySell.tsSell: xPosition.SetUpData(TradingPlatform.StateMarket.Ask);
              end;
            end;


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
  end;


  procedure _SetUpDataLevelBot;
  begin
    for var xBot in LevelBots do
    begin
      xBot.SetUpData(
        TradingPlatform.StateMarket.Ask,
        TradingPlatform.StateMarket.Bid
      );
    end;
  end;

  procedure _ShowLevelBot;
  var
    xBot: TLevelBot;
    i, iCount: Integer;
  begin
    iCount := LevelBots.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xBot := LevelBots[i];
        BotGrid.Cells[0,i] := (i + 1).ToString;
        BotGrid.Cells[1,i] := xBot.Profit.ToString;
        BotGrid.Cells[2,i] := xBot.Manager.Profit.ToString;
        BotGrid.Cells[3,i] := xBot.Triling.ToString;
        BotGrid.Cells[4,i] := xBot.Manager.Positions.Count.ToString;
        BotGrid.Cells[5,i] := xBot.Manager.ProfitFeeRatesTaker.ToString;
        BotGrid.Cells[6,i] := xBot.Manager.ProfitFeeRatesMaker.ToString;
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

  // **********************************************
  // Состояние бота
  _SetUpDataLevelBot;
  _ShowLevelBot;

  _ShowPosition;
end;

end.
