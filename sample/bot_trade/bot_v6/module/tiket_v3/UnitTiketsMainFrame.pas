unit UnitTiketsMainFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.SysUtils.Candel,
  Lb.Module.SysUtils,
  FMX.Objects,
  FMX.Layouts,
  FMX.ListBox, System.Rtti, FMX.Grid.Style, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Grid, FMX.Memo.Types, FMX.Memo;

const
  COUNT_TIKETS = 5000;

type
  TTiketList = TList<TTiket>;

  TTiketsMainFrameV2 = class(TFrame, IModule)
    Text: TText;
    StrGrid: TStringGrid;
    Memo: TMemo;
  private
    FTikets: TTiketList;
    FMemoryTikets: TMemoryTikets;
    FWeightedValues: TWeightedValueList;
    procedure SetGenerationEvent;
    procedure AddWeightedCount(const ACount: Integer);
    procedure SetTiket(const ATiket: TTiket);
    procedure SetTikets;
    procedure SetWeightedValue;
  protected
    function Start: Boolean;
    function Stop: Boolean;
    function UpData: Boolean;
    function GetCaption: WideString;
//    property MemoryTikets: TMemoryTikets read FMemoryTikets;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  Lb.Setting;

{ TTiketsMainFrame }

procedure TTiketsMainFrameV2.SetWeightedValue;

  procedure _SetWeightedValue(const AIndex: Integer; const ASumValue, ASumVol: Double);
  var
    xW: TWeightedValue;
  begin
    xW := FWeightedValues[AIndex];
    if ASumVol > 0 then
      xW.Price := Round(ASumValue/ASumVol);
    xW.Vol := ASumVol;

    if FMemoryTikets.Tiket.Price > xW.Price then
      xW.Status := 1
    else if FMemoryTikets.Tiket.Price < xW.Price then
      xW.Status := -1
    else
      xW.Status := 0;

    FWeightedValues[AIndex] := xW;
  end;

  function _IndexOfCount(const ACount: Integer): Integer;
  var
    xW: TWeightedValue;
    i, iCount: Integer;
  begin
    Result := -1;
    iCount := FWeightedValues.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xW := FWeightedValues[i];
        if xW.Count = ACount then
        begin
          Result := i;
          Break;
        end;
      end;
  end;

var
  xSumValue, xSumVol: Double;
  xTiket: TTiket;
  iCount, xInd: Integer;
begin
  xSumVol := 0;
  xSumValue := 0;
  iCount := 0;
  for var i := FTikets.Count - 1 downto 0 do
  begin
    xTiket := FTikets.Items[i];
    xSumVol := xSumVol + xTiket.Vol;
    xSumValue := xSumValue + xTiket.Value;
    Inc(iCount);
    xInd := _IndexOfCount(iCount);
    if xInd >= 0 then
      _SetWeightedValue(xInd,xSumValue,xSumVol);
  end;
end;

constructor TTiketsMainFrameV2.Create(AOwner: TComponent);

  procedure _AddColumn(const AName: String; const ASizeColumn: Integer = 150);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(StrGrid);
    xColumn.Header := AName;
    xColumn.Parent := StrGrid;
    xColumn.Width := ASizeColumn;
  end;

  procedure _SetInitializationColumn;
  begin
    _AddColumn('Count',80);
    _AddColumn('Price',100);
    _AddColumn('Vol',80);
    _AddColumn('Status',50);
    _AddColumn('SumStatus',50);
  end;

const
  WEIGHTED_COUNT = 100;

var
  xStep: Integer;
  i, iCount: Integer;
begin
  inherited Create(AOwner);
  FMemoryTikets := TMemoryTikets.Create;
  FTikets := TTiketList.Create;
  FWeightedValues := TWeightedValueList.Create;

  _SetInitializationColumn;

  iCount := 20;
  for i := 0 to iCount - 1 do
  begin
    xStep := WEIGHTED_COUNT * i + WEIGHTED_COUNT;
    AddWeightedCount(xStep);
  end;
  StrGrid.RowCount := FWeightedValues.Count;
end;

destructor TTiketsMainFrameV2.Destroy;
begin
  FreeAndNil(FWeightedValues);
  FreeAndNil(FTikets);
  FreeAndNil(FMemoryTikets);
  inherited;
end;

procedure TTiketsMainFrameV2.AddWeightedCount(const ACount: Integer);
var
  xW: TWeightedValue;
begin
  xW.Count := ACount;
  xW.Price := 0;
  xW.Vol := 0;
  xW.Status := 0;
  xW.SumStatus := 0;
  FWeightedValues.Add(xW);
end;

function TTiketsMainFrameV2.GetCaption: WideString;
begin
  Result := 'Торговая система на основе тикетов';
end;

function TTiketsMainFrameV2.Start: Boolean;
begin
  Result := True;
  FTikets.Clear;

  // Значение работы объекты
  FMemoryTikets.FileName := TSetting.ReadString(CONFIG_FILE_NAME);
  FMemoryTikets.First;
end;

function TTiketsMainFrameV2.Stop: Boolean;
begin
  Result := True;
end;

function TTiketsMainFrameV2.UpData: Boolean;
var
  xTiket: TTiket;
begin
  Result := True;
  try
    if FMemoryTikets.EOF then
    begin
      Result := False;
    end
    else
    begin
      xTiket := FMemoryTikets.Tiket;
      // Читаем тикер работы к
      SetTiket(xTiket);
      FMemoryTikets.Next;
    end;
  except
    Result := False;
  end;
end;

procedure TTiketsMainFrameV2.SetGenerationEvent;
var
  xTiket: TTiket;
  xSumStatus: Integer;
  xW: TWeightedValue;
  i, iCount: Integer;
begin
  xSumStatus := 0;
  iCount := FWeightedValues.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xW := FWeightedValues[i];
      xSumStatus := xSumStatus + xW.Status;
      xW.SumStatus := xSumStatus;
      FWeightedValues[i] := xW;
    end;
end;

procedure TTiketsMainFrameV2.SetTiket(const ATiket: TTiket);
begin
  // Получаем каждый новый тик
  if FTikets.Count > COUNT_TIKETS then
    FTikets.Delete(0);
  FTikets.Add(ATiket);
  Text.Text := ATiket.ToShortString;



  SetWeightedValue;
  SetTikets;
  SetGenerationEvent;
end;

procedure TTiketsMainFrameV2.SetTikets;
var
  xW: TWeightedValue;
  i, iCount: Integer;
begin
  StrGrid.BeginUpdate;
  try
    iCount := FWeightedValues.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xW := FWeightedValues[i];
        StrGrid.Cells[0,i] := xW.Count.ToString;
        StrGrid.Cells[1,i] := xW.Price.ToString;
        StrGrid.Cells[2,i] := xW.Vol.ToString;
        StrGrid.Cells[3,i] := xW.Status.ToString;
        StrGrid.Cells[4,i] := xW.SumStatus.ToString;
      end;
  finally
    StrGrid.EndUpdate;
  end;
end;

initialization
  RegistrationFrame('TIKETS',TTiketsMainFrameV2);

finalization

end.
