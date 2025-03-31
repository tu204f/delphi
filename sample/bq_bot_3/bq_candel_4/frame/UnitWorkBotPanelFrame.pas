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

  Lb.Bot.Candel,
  Lb.Breakdown,

  Lb.SysUtils,
  Lb.Bybit.SysUtils,
  Lb.Platform,
  Lb.Platform.Bybit,
  Lb.Journal.Trading;

type
  TWorkBotPanelFrame = class(TFrame)
    PositionGrid: TStringGrid;
    LayoutStatusMarket: TLayout;
    RectangleStatusMarket: TRectangle;
    EditNewCandel: TEdit;
    EditUpDataCandel: TEdit;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
  private
    FMainFormLog: IMainFormLog;

  private
    FWorkBot: TWorkBot;
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
  public
    procedure SetLog(S: String);
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
    SetAddColumn(PositionGrid,'OpenLinkID');
    SetAddColumn(PositionGrid,'CloseLinkID');
  end;

begin
  inherited;

  FCountNewCandel := 0;
  FCountUpDataCandel := 0;

  SetShowPositionGrid;

  FMainFormLog := nil;
  FWorkBot := TWorkBot.Create;
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

        AGrid.Cells[15,i] := xPosition.OpenLinkID;
        AGrid.Cells[16,i] := xPosition.CloseLinkID;

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

  procedure _SetUpProfit(AJournalManager: TJournalManager);
  var
    i, iCount: Integer;
    xPosition: TJournalPosition;
    xSumProfit, xSumProfitTaker, xSumProfitMaker: Double;
  begin
    xSumProfit := 0;
    xSumProfitTaker := 0;
    xSumProfitMaker := 0;

    iCount := AJournalManager.Positions.Count;
    if iCount > 0 then
      for i := iCount - 1 downto 0 do
      begin
        xPosition := AJournalManager.Positions[i];
        xSumProfit := xSumProfit + xPosition.Profit;
        xSumProfitTaker := xSumProfitTaker + xPosition.ProfitFeeRatesTaker;
        xSumProfitMaker := xSumProfitMaker + xPosition.ProfitFeeRatesMaker;
      end;

    Edit1.Text := xSumProfit.ToString;
    Edit2.Text := xSumProfitTaker.ToString;
    Edit3.Text := xSumProfitMaker.ToString;
  end;

begin
{$IFDEF DBG_STATE_MARKET}
  TLogger.LogTree(0,'TWorkBotPanelFrame.TradingPlatformStateMarket: Состояние рынка');
{$ENDIF}

  Inc(FCountUpDataCandel);
  EditNewCandel.Text := FCountNewCandel.ToString;
  EditUpDataCandel.Text := FCountUpDataCandel.ToString;

  if Assigned(FTradingPlatform) then
    FWorkBot.SetTradingPlatform(FTradingPlatform);

  _ShowJournalManager(PositionGrid,FWorkBot.JournalManager);
  _ShowJournalManagerProfit(FWorkBot.JournalManager);
  _SetUpDataPosition(FWorkBot.JournalManager);
  _SetUpProfit(FWorkBot.JournalManager);
end;

procedure TWorkBotPanelFrame.SetLog(S: String);
begin
  if Assigned(FMainFormLog) then
    FMainFormLog.LogMsg(S);
end;

procedure TWorkBotPanelFrame.SetTradingPlatform(const Value: TTradingPlatform);
begin
  FTradingPlatform := Value;
  FWorkBot.SetTradingPlatform(FTradingPlatform);
end;

end.
