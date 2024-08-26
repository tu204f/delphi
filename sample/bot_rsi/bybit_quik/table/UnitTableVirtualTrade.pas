unit UnitTableVirtualTrade;

interface

{$i platform.inc}

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
  Lb.VirtualTrade;

type
  TTableVirtualTradeFrame = class(TFrame)
    TableGrid: TStringGrid;
  private
    procedure VirtualTradesOnChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TTableVirtualTradeFrame }

constructor TTableVirtualTradeFrame.Create(AOwner: TComponent);

  procedure _AddColumn(AGrid: TStringGrid; ATitle: String; AWidth: Double = 80);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(nil);
    xColumn.Parent := AGrid;
    xColumn.Header := ATitle;
    xColumn.Width  := AWidth;
  end;

begin
  inherited Create(AOwner);
  TableGrid.ClearColumns;
  _AddColumn(TableGrid,'Time',120);
  _AddColumn(TableGrid,'Symbol');
  _AddColumn(TableGrid,'Side');
  _AddColumn(TableGrid,'Qty');
  _AddColumn(TableGrid,'Price');
  _AddColumn(TableGrid,'OrderLinkId',120);

  GetVirtualTrades.OnChange := VirtualTradesOnChange;
end;

destructor TTableVirtualTradeFrame.Destroy;
begin

  inherited;
end;

procedure TTableVirtualTradeFrame.VirtualTradesOnChange(Sender: TObject);
var
  iCount: Integer;
begin
  try
    iCount := 0;
    for var i := 0 to GetVirtualTrades.Count - 1 do
    begin
      var xPostionTrade := GetVirtualTrades.Items[i];
      for var j := 0 to xPostionTrade.Items.Count - 1 do
      begin

        var xParamTrade := xPostionTrade.Items[j];
        TableGrid.RowCount := iCount + 1;
        TableGrid.Cells[0,iCount] := DateTimeToStr(xParamTrade.Time);
        TableGrid.Cells[1,iCount] := xParamTrade.Symbol;
        TableGrid.Cells[2,iCount] := GetStrToTypeSide(xParamTrade.Side);
        TableGrid.Cells[3,iCount] := FloatToStr(xParamTrade.Qty);
        TableGrid.Cells[4,iCount] := FloatToStr(xParamTrade.Price);
        TableGrid.Cells[5,iCount] := xParamTrade.OrderLinkId;

        Inc(iCount);
      end;
    end;
  except
    raise Exception.Create('Error Message: Очень странная ошибка ее здесь не должно быть.');
  end;
end;

end.
