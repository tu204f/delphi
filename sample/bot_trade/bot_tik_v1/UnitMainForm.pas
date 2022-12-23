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
  Lb.SysUtils.Candel,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Objects, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.Edit, FMX.Layouts, System.Actions, FMX.ActnList,
  UnitValuesFrame;

type
  TMainForm = class(TForm)
    ButtonStartAndStop: TButton;
    Timer: TTimer;
    StrGrid: TStringGrid;
    StringColumnPrice: TStringColumn;
    StringColumnVol: TStringColumn;
    StringColumnIndex: TStringColumn;
    StringColumnInd: TStringColumn;
    Layout1: TLayout;
    procedure TimerTimer(Sender: TObject);
    procedure ButtonStartAndStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FIndexShow: Integer;
    MemoryTikets: TMemoryTikets;
    FBufferTikets: TTiketList;
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Math,
  System.DateUtils;

const
  BUFFER_TIKETS_COUNT = 40;

const
  FILE_NAME_SOURCE   = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\source.csv';
  FILE_NAME_TIKE     = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\SPFB.SBRF-12.22_221111_221111.csv';


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  MemoryTikets := TMemoryTikets.Create;
  FBufferTikets:= TTiketList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FBufferTikets);
  FreeAndNil(MemoryTikets);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  StrGrid.RowCount := BUFFER_TIKETS_COUNT;
end;

procedure TMainForm.ButtonStartAndStopClick(Sender: TObject);
begin
  FIndexShow := 0;
  FBufferTikets.Clear;

  MemoryTikets.FileName := FILE_NAME_TIKE;
  Timer.Enabled := not Timer.Enabled;
  case Timer.Enabled of
    True: begin
      ButtonStartAndStop.Text := 'Стоп';
      MemoryTikets.First;
    end;
    False: ButtonStartAndStop.Text := 'Старт';
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xIndexLast: Integer;

  function _SameFloat(const AValue1, AValue2: Double; AExactness: Integer = 0): Boolean;
  var
    xExactness: Integer;
    xValue1, xValue2: Integer;
  begin
    xExactness := Trunc(Power(10,AExactness));
    xValue1 := Trunc(AValue1 * xExactness);
    xValue2 := Trunc(AValue2 * xExactness);
    Result := xValue1 = xValue2;
  end;

  function _IndexOf(const APrice: Double): Integer;
  var
    xTiket: TTiket;
    i, iCount: Integer;
  begin
    Result := -1;
    iCount := FBufferTikets.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xTiket := FBufferTikets[i];
        if _SameFloat(xTiket.Price,APrice) then
        begin
          Result := i;
          Break;
        end;
      end;
  end;

  procedure _Insert(const ATiket: TTiket);
  var
    xB: Boolean;
    xTiket: TTiket;
    i, iCount: Integer;
  begin
    xB := True;
    iCount := FBufferTikets.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xTiket := FBufferTikets[i];
        if xTiket.Price < ATiket.Price then
        begin
          xB := False;
          xIndexLast := i;
          FBufferTikets.Insert(i,ATiket);
          Break;
        end;
      end;
      if xB then
        xIndexLast := FBufferTikets.Add(ATiket);
    end;
  end;


  procedure _ShowIndex(ATiket: TTiket);
  var
    xTiket: TTiket;
    xInd: Integer;
  begin
    if FBufferTikets.Count > 0 then
    begin
      // -------
      xTiket := FBufferTikets[FIndexShow];
      if xTiket.Price < ATiket.Price then
      begin
        FIndexShow := FIndexShow - 1;
        if FIndexShow < 0 then FIndexShow := 0;
      end;
      // --------
      xInd := FIndexShow + (BUFFER_TIKETS_COUNT - 1);
      if xInd < FBufferTikets.Count then
      begin
        xTiket := FBufferTikets[xInd];
        if xTiket.Price > ATiket.Price then
        begin
          FIndexShow := FIndexShow + 1;
          if FBufferTikets.Count < BUFFER_TIKETS_COUNT then
            FIndexShow := 0;
        end;
      end;

    end;
  end;

  procedure _SetAddBufferTiket(ATiket: TTiket);
  var
    iCount: Integer;
    xInd: Integer;
    xTiket: TTiket;
  begin
    iCount := FBufferTikets.Count;
    if iCount > 0 then
    begin
      xInd := _IndexOf(ATiket.Price);
      if xInd >= 0 then
      begin
        xTiket := FBufferTikets[xInd];
        xTiket.Vol := xTiket.Vol + ATiket.Vol;
        FBufferTikets[xInd] := xTiket;
      end
      else
        _Insert(ATiket);
    end
    else
      xIndexLast := FBufferTikets.Add(ATiket);

    _ShowIndex(ATiket);
  end;

  procedure _ShowBufferGrid;
  var
    xTiket: TTiket;
    i: Integer;
  begin
    StrGrid.RowCount := BUFFER_TIKETS_COUNT;
    for i := 0 to BUFFER_TIKETS_COUNT - 1 do
    begin
      if i >= FBufferTikets.Count then
        Break;

      xTiket := FBufferTikets[FIndexShow + i];
      StrGrid.Cells[0,i] := IntToStr(i);
      StrGrid.Cells[1,i] := FloatToStr(xTiket.Price);
      StrGrid.Cells[2,i] := FloatToStr(xTiket.Vol);


      if (FIndexShow + i) = xIndexLast then
        StrGrid.Cells[3,i] := '>'
      else
        StrGrid.Cells[3,i] := '';

    end;
  end;

var
  xTiket: TTiket;
begin
  try
    if not MemoryTikets.EOF then
    begin
      xTiket := MemoryTikets.Tiket;
      _SetAddBufferTiket(xTiket);
      MemoryTikets.Next;
    end
    else
      Timer.Enabled := False;
    _ShowBufferGrid;
  except
    on E : Exception do
      raise Exception.Create('Error Message:' + E.Message);
  end;
end;

end.
