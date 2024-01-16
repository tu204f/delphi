unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Memo.Types,
  FMX.Memo,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    StringGrid1: TStringGrid;
    Memo1: TMemo;
    procedure ButtonStartClick(Sender: TObject);
  private
  protected
    StartTime: String;
    BybitKline: TBybitKline;
    procedure BybitKlineOnEventEndLoading(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddColumn(const AHeader: String);
  var
    xColumn: TColumn;
  begin
    xColumn := TColumn.Create(StringGrid1);
    xColumn.Header := AHeader;
    xColumn.Parent := StringGrid1;
    xColumn.Width  := 120;
  end;

begin
  inherited;
  BybitKline := TBybitKline.Create;
  BybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;

  _AddColumn('startTime');
  _AddColumn('openPrice');
  _AddColumn('highPrice');
  _AddColumn('lowPrice');
  _AddColumn('closePrice');
  _AddColumn('volume');
  _AddColumn('turnover');
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(BybitKline);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  StartTime := '';
  BybitKline.Category := TTypeCategory.tcLinear;
  BybitKline.Symbol   := 'BTCUSDT';
  BybitKline.Interval := TTypeInterval.ti_5;
  BybitKline.Limit    := 5;
  BybitKline.Start(1000);
end;

procedure TMainForm.BybitKlineOnEventEndLoading(Sender: TObject);
var
  xCandel: TCandelObject;
  i, iCount: Integer;
begin
  StringGrid1.RowCount := 0;
  iCount := BybitKline.CandelObjects.Count;
  if iCount > 0 then
  begin
    StringGrid1.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      xCandel := BybitKline.CandelObjects[i];
      StringGrid1.Cells[0,i] := FormatDateTime('dd.mm.yy hh:nn:ss.zzz',xCandel.DateTime);

      StringGrid1.Cells[1,i] := xCandel.openPrice;
      StringGrid1.Cells[2,i] := xCandel.highPrice;
      StringGrid1.Cells[3,i] := xCandel.lowPrice;
      StringGrid1.Cells[4,i] := xCandel.closePrice;
      StringGrid1.Cells[5,i] := xCandel.volume;
      StringGrid1.Cells[6,i] := xCandel.turnover;
    end;
  end;
end;

end.
