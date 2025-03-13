unit UnitWorkBotPanelFrame;

interface

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
  Lb.SysUtils,
  Lb.Bybit.SysUtils,
  Lb.Platform,
  Lb.Platform.Bybit,
  Lb.Journal.Trading.v2;

type
  TWorkBotPanelFrame = class(TFrame)
    LayoutWork: TLayout;
    RectangleWork: TRectangle;
    EditRate: TEdit;
    LayoutWorkBot: TLayout;
    PositionGrid: TStringGrid;
    EditProfitFeeRatesMaker: TEdit;
    EditProfitFeeRatesTaker: TEdit;
    EditProfit: TEdit;
  private
    FMainFormLog: IMainFormLog;
    procedure SetLog(S: String);
  private
    FWorkBot: TWorkBotDeviation;
    FTradingPlatform: TTradingPlatform;
    FJournalManager: TJournalManager;
    FWorkBotFrame: TWorkBotFrame;
    procedure SetTradingPlatform(const Value: TTradingPlatform);
    function GetQty: Double;
  protected
    procedure PositionClose(ASander: TObject);
  protected
    procedure OpenPositionBuy;
    procedure OpenPositionSell;
    procedure ClosePosition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TradingPlatformNewCandel;
    procedure TradingPlatformStateMarket(AStateMarket: TStateMarket);
    property MainFormLog: IMainFormLog write FMainFormLog;
    property JournalManager: TJournalManager read FJournalManager;
    property WorkBot: TWorkBotDeviation write FWorkBot;
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
    SetAddColumn(PositionGrid,'Profit');
    SetAddColumn(PositionGrid,'TypeTrade');
    SetAddColumn(PositionGrid,'FeeRatesTaker');
    SetAddColumn(PositionGrid,'FeeRatesMaker');
  end;

begin
  inherited;
  SetShowPositionGrid;

  FMainFormLog := nil;
  FWorkBot := TWorkBotDeviation.Create;
  FWorkBot.OnCrossingValue := WorkBotOn—rossingLevel;


  FJournalManager := TJournalManager.Create;

  FWorkBotFrame := TWorkBotFrame.Create(nil);
  FWorkBotFrame.Parent := LayoutWorkBot;
  FWorkBotFrame.Align := TAlignLayout.Client;
  FWorkBotFrame.WorkBot := FWorkBot;

  LayoutWorkBot.Visible := False;
end;

destructor TWorkBotPanelFrame.Destroy;
begin
  FreeAndNil(FJournalManager);
  FreeAndNil(FWorkBot);
  inherited;
end;

procedure TWorkBotPanelFrame.TradingPlatformNewCandel;
begin
  LayoutWorkBot.Visible := True;
  FWorkBot.SetNewCandel;
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

        // œË·˚Î¸ Ò ‚ÓÁÏÓÊÌÓÈ ÍÓÏËÒÒËÂ
        AGrid.Cells[10,i] := FloatToStr(xPosition.Profit);
        AGrid.Cells[11,i] := FloatToStr(xPosition.ProfitFeeRatesTaker);
        AGrid.Cells[12,i] := FloatToStr(xPosition.ProfitFeeRatesMaker);
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
            TTypeBuySell.tsBuy: xPosition.SetUpData(TradingPlatform.StateMarket.Bid);
            TTypeBuySell.tsSell: xPosition.SetUpData(TradingPlatform.StateMarket.Ask);
          end;
        end;
      end;
  end;

var
  xRate: Double;
  xParam: TWorkBotDeviation.TParam;
begin
  _ShowJournalManager(PositionGrid,FJournalManager);

  EditRate.Text := '';
  if AStateMarket.Candels.Count > 0 then
  begin
    xRate := TradingPlatform.ValueVolatility.RsecommendRate;
    EditRate.Text := GetRound(xRate).ToString;

    xParam.Candel := AStateMarket.Candels[0];
    xParam.Deviation := TradingPlatform.ValueVolatility.DeviationValue;
    xParam.Rate := xRate * 0.2;
    FWorkBot.SetParamValue(xParam);
  end;

  _ShowJournalManagerProfit(FJournalManager);
  _SetUpDataPosition(FJournalManager);
end;

procedure TWorkBotPanelFrame.SetLog(S: String);
begin
  if Assigned(FMainFormLog) then
    FMainFormLog.LogMsg(S);
end;

procedure TWorkBotPanelFrame.WorkBotOn—rossingLevel(Sender: TObject; const AWorkBotStatus: TWorkBotStatus);
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TWorkBotPanelFrame.WorkBotOn—rossingLevel:');
  {$ENDIF}
  case AWorkBotStatus of
    TWorkBotStatus.wbsHigh: begin
      ClosePosition;
      OpenPositionBuy;
    end;
    TWorkBotStatus.wbsLow : begin
      ClosePosition;
      OpenPositionSell;
    end;
  end;
end;


procedure TWorkBotPanelFrame.SetTradingPlatform(const Value: TTradingPlatform);
begin
  FTradingPlatform := Value;
  FWorkBotFrame.TradingPlatform := FTradingPlatform;
end;

procedure TWorkBotPanelFrame.PositionClose(ASander: TObject);
var
  xPrice: Double;
  xPosition: TJournalPosition;
begin
  case xPosition.Side of
    TTypeBuySell.tsBuy : xPrice := xPosition.ClosePrice - 2;
    TTypeBuySell.tsSell: xPrice := xPosition.ClosePrice + 2;
  else
    xPrice := 0;
  end;
  if xPrice <= 0 then
    Exit;

  xPosition := TJournalPosition(ASander);
  TradingPlatform.SendTrade(
    xPosition.CloseTime,
    xPrice,
    1,
    xPosition.Side
  );
end;


function TWorkBotPanelFrame.GetQty: Double;
var
  xCount: INteger;
  xPosition: TJournalPosition;
begin
  Result := 1;
  xCount := JournalManager.Positions.Count;
  if xCount > 0 then
  begin
    xPosition := JournalManager.Positions[xCount - 1];
    if xPosition.TypeTrade = TTypeTrade.ttOpen then
      Result := 2;
  end;
end;

procedure TWorkBotPanelFrame.OpenPositionBuy;
var
  xQty  : Double;
  xPrice: Double;
  xPosition: TJournalPosition;
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TWorkBotPanelFrame.OpenPositionBuy:');
  {$ENDIF}
  xPrice := TradingPlatform.StateMarket.Ask;
  if xPrice > 0 then
  begin
    xQty := GetQty;
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
      Triling := 20;
      Profit  := 50;
      DoOpen;
    end;

    TradingPlatform.SendTrade(
      xPosition.OpenTime,
      xPosition.OpenPrice + 2,
      xQty,
      xPosition.Side
    );

  end;
end;

procedure TWorkBotPanelFrame.OpenPositionSell;
var
  xQty  : Double;
  xPrice: Double;
  xPosition: TJournalPosition;
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TWorkBotPanelFrame.OpenPositionSell:');
  {$ENDIF}
  xPrice := TradingPlatform.StateMarket.Bid;
  if xPrice > 0 then
  begin
    xQty := GetQty;
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
      Triling := 20;
      Profit  := 50;
      DoOpen;
    end;


    TradingPlatform.SendTrade(
      xPosition.OpenTime,
      xPosition.OpenPrice - 2,
      xQty,
      xPosition.Side
    );


  end;
end;

procedure TWorkBotPanelFrame.ClosePosition;
var
  xPrice: Double;
  xPosition: TJournalPosition;
  i, iCount: Integer;
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TWorkBotPanelFrame.ClosePosition:');
  {$ENDIF}
  iCount := JournalManager.Positions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPosition := JournalManager.Positions[i];
      if xPosition.TypeTrade = TTypeTrade.ttOpen then
      begin

        with xPosition do
        begin
          case Side of
            TTypeBuySell.tsBuy : xPrice := TradingPlatform.StateMarket.Bid;
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
end;

end.
