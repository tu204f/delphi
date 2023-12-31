unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline, FMX.Memo.Types, FMX.Memo;

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
  BybitKline.Interval := TTypeInterval.ti_1;
  BybitKline.Limit    := 5;
  BybitKline.Selected(1000);
end;

procedure TMainForm.BybitKlineOnEventEndLoading(Sender: TObject);
var
  xStartTime: String;
  i, iCount: Integer;
  xCandelObject: TCandelObject;
  xCandelObjects: TCandelObjectList;
begin
  xCandelObjects := TCandelObjectList.Create;
  try
    SetLinearObjects(BybitKline.ListJson,xCandelObjects);
    iCount := xCandelObjects.Count;
    if iCount > 0 then
    begin
      xStartTime := xCandelObjects[i].startTime;
      if not SameText(StartTime,xStartTime) then
      begin
        StartTime := xStartTime;
        Memo1.Lines.Add('Новаая свеча: ' + StartTime);
      end;

      StringGrid1.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xCandelObject := xCandelObjects[i];
        StringGrid1.Cells[0,i] := DateTimeToStr(xCandelObject.DateTime);
        StringGrid1.Cells[1,i] := xCandelObject.openPrice;
        StringGrid1.Cells[2,i] := xCandelObject.highPrice;
        StringGrid1.Cells[3,i] := xCandelObject.lowPrice;
        StringGrid1.Cells[4,i] := xCandelObject.closePrice;
        StringGrid1.Cells[5,i] := xCandelObject.volume;
        StringGrid1.Cells[6,i] := xCandelObject.turnover;
      end;
    end;
  finally
    FreeAndNil(xCandelObjects);
  end;
end;

end.
