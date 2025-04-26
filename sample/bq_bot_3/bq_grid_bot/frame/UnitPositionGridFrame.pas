unit UnitPositionGridFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,

  Lb.SysUtils,
  Lb.Journal.Trading;

type
  TPositionGridFrame = class(TFrame)
    PositionGrid: TStringGrid;
  private
    FJournalManager: TJournalManager;
    procedure ShowJournalManager(const AGrid: TStringGrid);
  protected
    property JournalManager: TJournalManager read FJournalManager;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpDataJournalManager(const AJournalManager: TJournalManager);
  end;

implementation

{$R *.fmx}

uses
  Lb.WorkBot.Grid;

{ TPositionGridFrame }

constructor TPositionGridFrame.Create(AOwner: TComponent);

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
  FJournalManager := nil;
  SetShowPositionGrid;
end;

destructor TPositionGridFrame.Destroy;
begin

  inherited;
end;

procedure TPositionGridFrame.ShowJournalManager(const AGrid: TStringGrid);
var
  i, Count: Integer;
  xPosition: TJournalPosition;
begin
  Count := JournalManager.Positions.Count;
  AGrid.RowCount := Count;

  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xPosition := JournalManager.Positions[i];
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

procedure TPositionGridFrame.UpDataJournalManager(const AJournalManager: TJournalManager);
begin
  FJournalManager := AJournalManager;
  if Assigned(FJournalManager) then
    ShowJournalManager(PositionGrid);
end;

end.
