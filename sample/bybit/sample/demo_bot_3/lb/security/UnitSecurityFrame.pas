unit UnitSecurityFrame;

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
  Lb.TableSCV;

type
  ///<summary>
  /// “аблица инструментов, выбираем инструмент
  ///</summary>
  TSecurityFrame = class(TFrame)
    StrGrid: TStringGrid;
  private
    Table: TTableSCV;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLoadTable;
  end;

implementation

{$R *.fmx}


{ TSecurityFrame }

constructor TSecurityFrame.Create(AOwner: TComponent);
begin
  inherited;
  Table := TTableSCV.Create;
  SetLoadTable;
end;

destructor TSecurityFrame.Destroy;
begin
  FreeAndNil(Table);
  inherited;
end;

procedure TSecurityFrame.SetLoadTable;

  procedure _SetAddColumn(const AHeader: String);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(StrGrid);
    xColumn.Header := AHeader;
    xColumn.Parent := StrGrid;
  end;

  procedure _SetCreateColumn;
  begin
    for var xField in Table.Fields do
      _SetAddColumn(xField.Name);
  end;

  procedure _SetShowColumn;
  var
    xS: String;
  begin
    StrGrid.RowCount := Table.RowCount;
    for var C := 0 to Table.ColCount - 1 do
      for var R := 0 to Table.RowCount - 1 do
      begin
        xS := Table.Cells[C,R];
        StrGrid.Cells[C,R] := xS;
      end;
  end;

begin
  Table.Load('instrument.csv');
  _SetCreateColumn;
  _SetShowColumn;
end;

end.
