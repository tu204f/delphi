unit UnitPositionFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox,
  Lb.SysUtils,
  Lb.Bot,
  Lb.Journal.Trading,
  FMX.Grid;

type
  TPositionFrame = class(TFrame)
    PositionGrid: TStringGrid;
  private
    FWorkBot: TWorkBot;
    FProfit: Double;
    FProfitFeeRatesTaker: Double;
    FProfitFeeRatesMaker: Double;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetTradingPlatform(const ASourceLine: TSourceLine);
    property WorkBot: TWorkBot read FWorkBot write FWorkBot;
  public
    property Profit: Double read FProfit;
    property ProfitFeeRatesTaker: Double read FProfitFeeRatesTaker;
    property ProfitFeeRatesMaker: Double read FProfitFeeRatesMaker;
  end;

implementation

{$R *.fmx}

{ TPositionFrame }

constructor TPositionFrame.Create(AOwner: TComponent);

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
  FWorkBot := nil;
  SetShowPositionGrid;
end;

destructor TPositionFrame.Destroy;
begin

  inherited;
end;

procedure TPositionFrame.SetTradingPlatform(const ASourceLine: TSourceLine);

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

    FProfit := xSummProfit;
    FProfitFeeRatesTaker := xSummProfitFeeRatesTaker;
    FProfitFeeRatesMaker := xSummProfitFeeRatesMaker;
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
            TTypeBuySell.tsBuy: xPosition.SetUpData(ASourceLine.Bid);
            TTypeBuySell.tsSell: xPosition.SetUpData(ASourceLine.Ask);
          end;
        end;
      end;
  end;

begin
  if Assigned(FWorkBot) then
  begin
    _ShowJournalManager(PositionGrid,FWorkBot.JournalManager);
    _ShowJournalManagerProfit(FWorkBot.JournalManager);
    _SetUpDataPosition(FWorkBot.JournalManager);
  end;
end;

end.
