unit UnitWorkBotPanelFrame;

interface

{$I debug_volt.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,

  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Objects,
  FMX.Layouts,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,

  UnitWorkBotFrame,

  Lb.Bot.V4,
  Lb.Breakdown,

  Lb.SysUtils,
  Lb.Bybit.SysUtils,
  Lb.Platform,
  Lb.Platform.Bybit,
  Lb.Journal.Trading.v2;

type
  TWorkBotPanelFrame = class(TFrame)
    LayoutWork: TLayout;
    RectangleWork: TRectangle;
    EditDeviationValue: TEdit;
    LayoutWorkBot: TLayout;
    PositionGrid: TStringGrid;
    EditProfitFeeRatesMaker: TEdit;
    EditProfitFeeRatesTaker: TEdit;
    EditProfit: TEdit;
    LayoutStatusMarket: TLayout;
    RectangleStatusMarket: TRectangle;
    EditNewCandel: TEdit;
    EditUpDataCandel: TEdit;
  private
    FMainFormLog: IMainFormLog;
    procedure SetLog(S: String);
  private
    FWorkBot: TWorkBot;
    FWorkBotFrame: TWorkBotFrame;
    FTradingPlatform: TTradingPlatform;
    procedure SetTradingPlatform(const Value: TTradingPlatform);
  protected
    FCountNewCandel: Integer;
    FCountUpDataCandel: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure TradingPlatformNewCandel;
    procedure TradingPlatformStateMarket(AStateMarket: TStateMarket);

    property MainFormLog: IMainFormLog write FMainFormLog;
    property WorkBot: TWorkBot read FWorkBot write FWorkBot;
    property TradingPlatform: TTradingPlatform read FTradingPlatform write SetTradingPlatform;
  end;

implementation

{$R *.fmx}

uses
  Lb.Logger;

{ TWorkBotPanelFrame }

constructor TWorkBotPanelFrame.Create(AOwner: TComponent);

  procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := AStrGrid;
    xCol.Header := AHeader;
    xCol.Width  := AWidth;
  end;

  procedure SetShowPositionGrid;
  begin
    SetAddColumn(PositionGrid,'id',50);
    SetAddColumn(PositionGrid,'OpenTime',120);
    SetAddColumn(PositionGrid,'OpenPrice');
    SetAddColumn(PositionGrid,'CloseTime',120);
    SetAddColumn(PositionGrid,'ClosePrice');
    SetAddColumn(PositionGrid,'Qty');
    SetAddColumn(PositionGrid,'Side');
    SetAddColumn(PositionGrid,'SL');
    SetAddColumn(PositionGrid,'TK');
    SetAddColumn(PositionGrid,'TypeTrade');
    SetAddColumn(PositionGrid,'Profit');
    SetAddColumn(PositionGrid,'MaxProfit');
    SetAddColumn(PositionGrid,'MinProfit');
    SetAddColumn(PositionGrid,'FeeRatesTaker');
    SetAddColumn(PositionGrid,'FeeRatesMaker');
  end;

begin
  inherited;

  FCountNewCandel := 0;
  FCountUpDataCandel := 0;

  SetShowPositionGrid;

  FMainFormLog := nil;
  FWorkBot := TWorkBot.Create;
  FWorkBot.Rate := 1;

  FWorkBotFrame := TWorkBotFrame.Create(nil);
  FWorkBotFrame.Parent := LayoutWorkBot;
  FWorkBotFrame.Align := TAlignLayout.Client;
  FWorkBotFrame.Breakdown := FWorkBot.Breakdown;

  LayoutWorkBot.Visible := False;
end;

destructor TWorkBotPanelFrame.Destroy;
begin
  FreeAndNil(FWorkBot);
  inherited;
end;

procedure TWorkBotPanelFrame.TradingPlatformNewCandel;
begin
  Inc(FCountNewCandel);
  FCountUpDataCandel := 0;

  EditNewCandel.Text := FCountNewCandel.ToString;
  EditUpDataCandel.Text := FCountUpDataCandel.ToString;

  if Assigned(FTradingPlatform) then
    FWorkBot.SetTradingNewCandel;
end;

procedure TWorkBotPanelFrame.TradingPlatformStateMarket(AStateMarket: TStateMarket);

  procedure _ShowJournalManager(const AGrid: TStringGrid; const AJournalManager: TJournalManager);
  var
    i, Count: Integer;
    xPosition: TJournalPosition;
  begin
    Count := AJournalManager.Positions.Count;
    AGrid.RowCount := Count;

    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xPosition := AJournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttClose then
          xPosition.SetUpdata;

        AGrid.Cells[0,i] := (i + 1).ToString;
        AGrid.Cells[1,i] := DateTimeToStr(xPosition.OpenTime);
        AGrid.Cells[2,i] := FloatToStr(xPosition.OpenPrice);

        if xPosition.ClosePrice = 0 then
        begin
          AGrid.Cells[3,i] := '';
          AGrid.Cells[4,i] := '';
        end else
        begin
          AGrid.Cells[3,i] := DateTimeToStr(xPosition.CloseTime);
          AGrid.Cells[4,i] := FloatToStr(xPosition.ClosePrice);
        end;

        AGrid.Cells[5,i] := FloatToStr(xPosition.Qty);
        AGrid.Cells[6,i] := GetStrToSide(xPosition.Side);
        AGrid.Cells[7,i] := FloatToStr(xPosition.StopLoss);
        AGrid.Cells[8,i] := FloatToStr(xPosition.TakeProfit);
        AGrid.Cells[9,i] := GetStrToTypeTrade(xPosition.TypeTrade);

        // Прибыль с возможной комиссие
        AGrid.Cells[10,i] := FloatToStr(xPosition.Profit);
        AGrid.Cells[11,i] := FloatToStr(xPosition.MaxProfit);
        AGrid.Cells[12,i] := FloatToStr(xPosition.MinProfit);
        AGrid.Cells[13,i] := FloatToStr(xPosition.ProfitFeeRatesTaker);
        AGrid.Cells[14,i] := FloatToStr(xPosition.ProfitFeeRatesMaker);
      end;
  end;

  procedure _ShowJournalManagerProfit(const AJournalManager: TJournalManager);
  var
    i, Count: Integer;
    xPosition: TJournalPosition;
    xSummProfit, xSummProfitFeeRatesTaker, xSummProfitFeeRatesMaker: Double;
  begin
    xSummProfit := 0;
    xSummProfitFeeRatesTaker := 0;
    xSummProfitFeeRatesMaker := 0;

    Count := AJournalManager.Positions.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xPosition := AJournalManager.Positions[i];
        xSummProfit := xSummProfit + xPosition.Profit;
        xSummProfitFeeRatesTaker := xSummProfitFeeRatesTaker + xPosition.ProfitFeeRatesTaker;
        xSummProfitFeeRatesMaker := xSummProfitFeeRatesMaker + xPosition.ProfitFeeRatesMaker;
      end;

    EditProfit.Text := xSummProfit.ToString;
    EditProfitFeeRatesTaker.Text := xSummProfitFeeRatesTaker.ToString;
    EditProfitFeeRatesMaker.Text := xSummProfitFeeRatesMaker.ToString;
  end;

  procedure _SetUpDataPosition(AJournalManager: TJournalManager);
  var
    i, iCount: Integer;
    xPosition: TJournalPosition;
  begin
    iCount := AJournalManager.Positions.Count;
    if iCount > 0 then
      for i := iCount - 1 downto 0 do
      begin
        xPosition := AJournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttOpen then
        begin
          case xPosition.Side of
            TTypeBuySell.tsBuy: xPosition.SetUpData(FTradingPlatform.StateMarket.Bid);
            TTypeBuySell.tsSell: xPosition.SetUpData(FTradingPlatform.StateMarket.Ask);
          end;
        end;
      end;
  end;

var
  xRate: Double;
  xCandel: TCandel;
begin
{$IFDEF DBG_STATE_MARKET}
  TLogger.LogTree(0,'TWorkBotPanelFrame.TradingPlatformStateMarket: Состояние рынка');
{$ENDIF}
  LayoutWorkBot.Visible := True;

  Inc(FCountUpDataCandel);
  EditNewCandel.Text := FCountNewCandel.ToString;
  EditUpDataCandel.Text := FCountUpDataCandel.ToString;

  if Assigned(FTradingPlatform) then
  begin
    EditDeviationValue.Text := FTradingPlatform.ValueVolatility.DeviationValue.ToString;
    FWorkBot.SetTradingPlatform(FTradingPlatform);
  end;

  _ShowJournalManager(PositionGrid,FWorkBot.JournalManager);
  _ShowJournalManagerProfit(FWorkBot.JournalManager);
  _SetUpDataPosition(FWorkBot.JournalManager);
end;

procedure TWorkBotPanelFrame.SetLog(S: String);
begin
  if Assigned(FMainFormLog) then
    FMainFormLog.LogMsg(S);
end;

procedure TWorkBotPanelFrame.SetTradingPlatform(const Value: TTradingPlatform);
begin
  FTradingPlatform := Value;
  FWorkBotFrame.TradingPlatform := FTradingPlatform;
  FWorkBot.SetTradingPlatform(FTradingPlatform);
end;

end.
