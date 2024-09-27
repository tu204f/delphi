unit UnitBybitExportFrame;

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
  FMX.Layouts,
  FMX.Objects,
  FMX.ListBox,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  Lb.Status,
  Lb.Status.Bybit,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  Lb.Indicator;

type
  TBybitExportFrame = class(TFrame)
    LayoutClient: TLayout;
    LayoutTop: TLayout;
    TextTable: TText;
    ComboBoxTable: TComboBox;
    StrGrid: TStringGrid;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
    FBybitStatus: TBybitStatus;
    procedure SetBybitStatusHistory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BybitStatus: TBybitStatus read FBybitStatus write FBybitStatus;
  end;

implementation

{$R *.fmx}

{ TBybitExportFrame }

constructor TBybitExportFrame.Create(AOwner: TComponent);
begin
  inherited;
  FBybitStatus := nil;
end;

destructor TBybitExportFrame.Destroy;
begin

  inherited;
end;


procedure TBybitExportFrame.SetBybitStatusHistory;

  procedure _AddCol(AGrid: TStringGrid; ATitle: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := AGrid;
    xCol.Header := ATitle;
  end;

  procedure _ShowGrid(AGrid: TStringGrid);
  var
    i, iCount: Integer;
    xCandelObject: TCandelObject;
    xCandels: TCandelObjectList;
    xValueRSI: TRSI_V2;
    xTrandRSI: TRSI_V2;
  begin
    xCandels := FBybitStatus.HistoryIndicator.Candels;
    xValueRSI := FBybitStatus.HistoryIndicator.ValueRSI;
    xTrandRSI := FBybitStatus.HistoryIndicator.TrandRSI;

    iCount := xCandels.Count;
    if iCount > 0 then
    begin
      AGrid.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xCandelObject := xCandels[i];

        AGrid.Cells[0,i] := DateTimeToStr(xCandelObject.DateTime);
        AGrid.Cells[1,i] := xCandelObject.Open.ToString;
        AGrid.Cells[2,i] := xCandelObject.High.ToString;
        AGrid.Cells[3,i] := xCandelObject.Low.ToString;
        AGrid.Cells[4,i] := xCandelObject.Close.ToString;
        AGrid.Cells[5,i] := xCandelObject.Vol.ToString;
        AGrid.Cells[6,i] := xValueRSI.Values[i].ToString;
        AGrid.Cells[7,i] := xTrandRSI.Values[i].ToString;
      end;

    end;
  end;

begin
  StrGrid.BeginUpdate;
  try
    _AddCol(StrGrid,'Дате/Время');
    _AddCol(StrGrid,'Open');
    _AddCol(StrGrid,'High');
    _AddCol(StrGrid,'Low');
    _AddCol(StrGrid,'Close');
    _AddCol(StrGrid,'Vol');
    _AddCol(StrGrid,'FastRSI');
    _AddCol(StrGrid,'SlowRSI');
  finally
    StrGrid.EndUpdate;
  end;
end;

procedure TBybitExportFrame.TimerTimer(Sender: TObject);
begin
  if not Assigned(FBybitStatus) then
    Exit;

  SetBybitStatusHistory;

end;

end.
