unit UnitBarsFrame;

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
  FMX.Objects,
  UnitBarFrame,
  Lb.Pattern,
  Lb.ReadPrice;

type
  ///<summary>Показываем оба графика</summary>
  TBarsFrame = class(TFrame)
    Timer: TTimer;
    Rectangle: TRectangle;
    procedure TimerTimer(Sender: TObject);
  private
    FBarFrames: TBarFrameList;
    procedure SetCreateBarFrames;
    //function GetCountBars: Integer;
  private
    FMaxValue: Double;
    FMinValue: Double;
    FCandels: TCandelList;
    procedure SetMaxAndMinValue(var AMaxValue, AMinValue: Double);
    function NewBar(const AIndex: Integer): TBarFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetShowStructure(const ACandels: TCandelList);
    procedure SetShowParants(const AParrentCandels: TParrentCandelList);
  end;

implementation

{$R *.fmx}

uses
  UnitMainForm;

{ TBarsFrame }

constructor TBarsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCandels := TCandelList.Create;
  FBarFrames := TBarFrameList.Create;
end;

destructor TBarsFrame.Destroy;
begin
  FreeAndNil(FCandels);
  FreeAndNil(FBarFrames);
  inherited;
end;

procedure TBarsFrame.SetCreateBarFrames;
var
  i: Integer;
  xDelta: Single;
  xBar: TBarFrame;
  xX, xY, xWidth, xHeight: Single;
  iCountBars: Integer;
begin
  iCountBars := FBarFrames.Count;
  if iCountBars > 0 then
  begin
    xDelta := (Self.Width - 2)/iCountBars;
    for i := 0 to iCountBars - 1 do
    begin
      xBar := FBarFrames[i];

      if i = 0 then
        xX := xDelta * i + 1
      else
        xX := xDelta * i;

      xY := 0;
      xWidth := xDelta;
      xHeight := Self.Height;

      xBar.SetBounds(xX,xY,xWidth,xHeight);
      xBar.SetShowCandel;

    end;
  end;
end;

procedure TBarsFrame.TimerTimer(Sender: TObject);
begin
  SetCreateBarFrames;
end;

procedure TBarsFrame.SetMaxAndMinValue(var AMaxValue, AMinValue: Double);
var
  i, iCount: Integer;
  xCandel: TCandel;
begin
  AMaxValue := 0;
  AMinValue := 0;

  iCount := FCandels.Count;
  if iCount > 0 then
  begin
    xCandel := FCandels.Items[0];
    AMaxValue := xCandel.High;
    AMinValue := xCandel.Low;
    for i := 1 to iCount - 1 do
    begin
      xCandel := FCandels[i];
      if xCandel.High > AMaxValue then AMaxValue := xCandel.High;
      if xCandel.Low  < AMinValue then AMinValue := xCandel.Low;
    end;
  end;
end;

function TBarsFrame.NewBar(const AIndex: Integer): TBarFrame;
var
  xBar: TBarFrame;
begin
  if AIndex >= FBarFrames.Count then
  begin
    xBar := TBarFrame.Create(nil);
    xBar.Parent := Rectangle;
    FBarFrames.Add(xBar);
    Result := xBar;
  end
  else
  begin
    xBar := FBarFrames[AIndex];
    Result := xBar;
  end;
end;


procedure TBarsFrame.SetShowParants(const AParrentCandels: TParrentCandelList);

  procedure SetCopyParrentCandel(const AParrentCandels: TParrentCandelList);
  var
    xParrentCandel: TParrentCandel;
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    FCandels.Clear;
    iCount := AParrentCandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xParrentCandel := AParrentCandels[i];

        xCandel.Open  := xParrentCandel.Open;
        xCandel.High  := xParrentCandel.High;
        xCandel.Low   := xParrentCandel.Low;
        xCandel.Close := xParrentCandel.Close;
        xCandel.Vol   := xParrentCandel.Vol;

        FCandels.Add(xCandel);
      end;
  end;

var
  i, iCount: Integer;
  xBar: TBarFrame;
  xCandel: TCandel;
begin
  FMaxValue := 0;
  FMinValue := 0;

  SetCopyParrentCandel(AParrentCandels);

  SetMaxAndMinValue(FMaxValue,FMinValue);
  iCount := FCandels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xBar := NewBar(i);
      xCandel := FCandels[i];
      xBar.SetCandel(FMaxValue,FMinValue,xCandel);
    end;
end;

procedure TBarsFrame.SetShowStructure(const ACandels: TCandelList);

  procedure SetCopyCandel(const ACandels: TCandelList);
  var
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    FCandels.Clear;
    iCount := ACandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        FCandels.Add(xCandel);
      end;
  end;

var
  i, iCount: Integer;
  xBar: TBarFrame;
  xCandel: TCandel;
begin
  FMaxValue := 0;
  FMinValue := 0;

  BeginUpdate;
  try

    SetCopyCandel(ACandels);

    SetMaxAndMinValue(FMaxValue,FMinValue);
    iCount := FCandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xBar := NewBar(i);
        xCandel := FCandels[i];
        xBar.SetCandel(FMaxValue,FMinValue,xCandel);
      end;

  finally
    EndUpdate;
  end;
end;

end.
