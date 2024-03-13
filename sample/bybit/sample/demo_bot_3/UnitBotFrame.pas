unit UnitBotFrame;

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
  FMX.StdCtrls, System.Rtti, FMX.Grid.Style, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Grid, FMX.Edit, FMX.Layouts,
  Lb.Mode,
  Lb.Position.Trade, FMX.Menus;

type
  TBotFrame = class(TFrame)
    StrGrid: TStringGrid;
    Edit1: TEdit;
    PopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    procedure MenuItem1Click(Sender: TObject);
  private
    procedure SetShowGrid;
  public
    TradePostions: TTradePostions;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function UpDataTimer: Boolean;
    procedure SetUpPrice(ATime: Int64; APrice: Double);
  end;

implementation

{$R *.fmx}

{ TBotFrame }

constructor TBotFrame.Create(AOwner: TComponent);

  procedure _SetAddCol(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(AStrGrid);
    xCol.Header := AHeader;
    xCol.Parent := AStrGrid;
    xCol.Width := AWidth;
  end;

  procedure _SetHeaders(const AGrid: TStringGrid);
  begin
    _SetAddCol(AGrid,'ID',50);
    _SetAddCol(AGrid,'OpenTime',120);
    _SetAddCol(AGrid,'CloseTime',120);
    _SetAddCol(AGrid,'Open');
    _SetAddCol(AGrid,'Close');
    _SetAddCol(AGrid,'Mode');
    _SetAddCol(AGrid,'Quantity');
    _SetAddCol(AGrid,'Status');
    _SetAddCol(AGrid,'TakeProfit');
    _SetAddCol(AGrid,'StopLoss');
    _SetAddCol(AGrid,'PriceTP');
    _SetAddCol(AGrid,'PriceSL');
    _SetAddCol(AGrid,'Profit');
    _SetAddCol(AGrid,'StopProfit');
    _SetAddCol(AGrid,'MaxProfit');
    _SetAddCol(AGrid,'MinProfit');
    _SetAddCol(AGrid,'ProfitQuantity');
    _SetAddCol(AGrid,'CommissionValue');
    _SetAddCol(AGrid,'IsTakeProfit');
    _SetAddCol(AGrid,'IsStopLoss');
  end;

begin
  inherited;
  TradePostions := TTradePostions.Create;
  _SetHeaders(StrGrid);
end;

destructor TBotFrame.Destroy;
begin

  inherited;
end;


procedure TBotFrame.SetShowGrid;

  function _IfThen(AValue: Boolean): String;
  begin
    if AValue then
      Result := 'True'
    else
      Result := 'False'
  end;

  procedure _RowGrid(AGrid: TStringGrid; APostion: TTradePostion; ARowID: Integer);
  begin
    AGrid.Cells[0,ARowID]  := APostion.ID.ToString;
    AGrid.Cells[1,ARowID]  := FloatToStr(APostion.OpenTime);
    AGrid.Cells[2,ARowID]  := FloatToStr(APostion.CloseTime);
    AGrid.Cells[3 ,ARowID] := APostion.Open.ToString;
    AGrid.Cells[4 ,ARowID] := APostion.Close.ToString;
    AGrid.Cells[5 ,ARowID] := GetStrToMode(APostion.Mode);
    AGrid.Cells[6 ,ARowID] := APostion.Quantity.ToString;
    AGrid.Cells[7 ,ARowID] := GetStrToStaus(APostion.Status);
    AGrid.Cells[8 ,ARowID] := APostion.TakeProfit.ToString;
    AGrid.Cells[9 ,ARowID] := APostion.StopLoss.ToString;
    AGrid.Cells[10,ARowID] := APostion.PriceTP.ToString;
    AGrid.Cells[11,ARowID] := APostion.PriceSL.ToString;
    AGrid.Cells[12,ARowID] := APostion.Profit.ToString;
    AGrid.Cells[13,ARowID] := APostion.StopProfit.ToString;
    AGrid.Cells[14,ARowID] := APostion.MaxProfit.ToString;
    AGrid.Cells[15,ARowID] := APostion.MinProfit.ToString;
    AGrid.Cells[16,ARowID] := APostion.ProfitQuantity.ToString;
    AGrid.Cells[17,ARowID] := APostion.CommissionValue.ToString;
    AGrid.Cells[18,ARowID] := _IfThen(APostion.IsTakeProfit);
    AGrid.Cells[19,ARowID] := _IfThen(APostion.IsStopLoss);
  end;

  procedure _ClearGrid(AGrid: TStringGrid; ARowID: Integer);
  var
    i, iCount: Integer;
  begin
    iCount := AGrid.ColumnCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
        AGrid.Cells[i,ARowID] := '';
  end;

var
  xPostion: TTradePostion;
  i, iCount: Integer;
begin
  iCount := TradePostions.Items.Count;
  if iCount > 0 then
  begin
    StrGrid.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      xPostion := TradePostions.Items[iCount - 1 - i];
      _RowGrid(StrGrid,xPostion,i);
      if i >= (StrGrid.RowCount - 1) then
        Break;
    end;
  end;
end;

procedure TBotFrame.MenuItem1Click(Sender: TObject);

  function _IfThen(AValue: Boolean): String;
  begin
    if AValue then
      Result := 'True'
    else
      Result := 'False'
  end;

  function _Row(APostion: TTradePostion): String;
  var
    xS: String;
  begin
    xS := APostion.ID.ToString + ';' +
          FloatToStr(APostion.OpenTime) + ';' +
          FloatToStr(APostion.CloseTime) + ';' +
          APostion.Open.ToString + ';' +
          APostion.Close.ToString + ';' +
          GetStrToMode(APostion.Mode) + ';' +
          APostion.Quantity.ToString + ';' +
          GetStrToStaus(APostion.Status) + ';' +
          APostion.TakeProfit.ToString + ';' +
          APostion.StopLoss.ToString + ';' +
          APostion.PriceTP.ToString + ';' +
          APostion.PriceSL.ToString + ';' +
          APostion.Profit.ToString + ';' +
          APostion.StopProfit.ToString + ';' +
          APostion.MaxProfit.ToString + ';' +
          APostion.MinProfit.ToString + ';' +
          APostion.ProfitQuantity.ToString + ';' +
          APostion.CommissionValue.ToString + ';' +
          _IfThen(APostion.IsTakeProfit) + ';' +
          _IfThen(APostion.IsStopLoss);
    Result := xS;
  end;

var
  xPostion: TTradePostion;
  i, iCount: Integer;
  xStr: TStrings;
begin
  iCount := TradePostions.Items.Count;
  if iCount > 0 then
  begin
    xStr := TStringList.Create;
    try
      for i := 0 to iCount - 1 do
      begin
        xPostion := TradePostions.Items[i];
        xStr.Add(_Row(xPostion));
      end;
      xStr.SaveToFile('positions.csv');
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

function TBotFrame.UpDataTimer: Boolean;
begin
  Result := True;
  SetShowGrid;
end;

procedure TBotFrame.SetUpPrice(ATime: Int64; APrice: Double);
begin
  TradePostions.SetUpData(
      ATime,
      APrice
  );
  Edit1.Text := TradePostions.GetProfit(APrice).ToString;
end;

end.
