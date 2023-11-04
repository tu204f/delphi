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
  Lb.SysUtils.Candel;

type
  ///<summary>Показываем оба графика</summary>
  TBarsFrame = class(TFrame)
    Timer: TTimer;
    Rectangle: TRectangle;
    procedure TimerTimer(Sender: TObject);
  public const
    COUNT_SOURCE = 10;
    COUNT_FUTURE = 10;
  private
    FBarFrames: TBarFrameList;
    procedure SetCreateBarFrames;
    //function GetCountBars: Integer;
  private
    FMaxValue: Double;
    FMinValue: Double;
    FStructure: TStructure;
    procedure SetMaxAndMinValue(var AMaxValue, AMinValue: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetShowStructure(const AStructure: TStructure);
  end;

implementation

{$R *.fmx}

uses
  UnitMainForm;

{ TBarsFrame }

constructor TBarsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStructure := nil;
  FBarFrames := TBarFrameList.Create;
end;

destructor TBarsFrame.Destroy;
begin
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

  //{$DEFINE DEFAULT_MAX_MIN_VALUE}

  {$IFDEF DEFAULT_MAX_MIN_VALUE}
  {$ELSE}
  procedure _SetMaxAndMin(const ACandels: TCandelList; var AMaxValue, AMinValue: Double);
  var
    i, iCount: Integer;
    xCandel: TCandel;
  begin
    iCount := ACandels.Count;
    if iCount > 0 then
    begin
      xCandel := ACandels.Items[0];

      if AMaxValue = 0 then
         AMaxValue := xCandel.High
      else
        if xCandel.High > AMaxValue then
          AMaxValue := xCandel.High;

      if AMinValue = 0 then
        AMinValue := xCandel.Low
      else
        if xCandel.Low  < AMinValue then
          AMinValue := xCandel.Low;

      for i := 1 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        if xCandel.High > AMaxValue then
          AMaxValue := xCandel.High;
        if xCandel.Low  < AMinValue then
          AMinValue := xCandel.Low;
      end;
    end;
  end;
  {$ENDIF}

{$IFDEF DEFAULT_MAX_MIN_VALUE}
{$ELSE}
var
  i, iCount: Integer;
  xCandel: TCandel;
{$ENDIF}
begin
{$IFDEF DEFAULT_MAX_MIN_VALUE}
  AMaxValue := 200;
  AMinValue := 0;
{$ELSE}
  AMaxValue := 0;
  AMinValue := 0;

  iCount := FStructure.SourceVectors.Count;
  if iCount > 0 then
  begin
    xCandel := FStructure.SourceVectors.Items[0];
    AMaxValue := xCandel.High;
    AMinValue := xCandel.Low;
    for i := 1 to iCount - 1 do
    begin
      xCandel := FStructure.SourceVectors[i];
      if xCandel.High > AMaxValue then AMaxValue := xCandel.High;
      if xCandel.Low  < AMinValue then AMinValue := xCandel.Low;
    end;
  end;

  iCount := FStructure.FutureVectors.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
    begin
      xCandel := FStructure.FutureVectors[i];
      if xCandel.High > AMaxValue then AMaxValue := xCandel.High;
      if xCandel.Low  < AMinValue then AMinValue := xCandel.Low;
    end;
  end;
{$ENDIF}

end;

procedure TBarsFrame.SetShowStructure(const AStructure: TStructure);

  function _NewBar(const AIndex: Integer): TBarFrame;
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

var
  i, iCount: Integer;
  xBar: TBarFrame;
  xCandel: TCandel;
  xIndex: Integer;
begin
  FMaxValue := 0;
  FMinValue := 0;

  FStructure := AStructure;


  // FBarFrames.Clear;

  SetMaxAndMinValue(FMaxValue,FMinValue);

  xIndex := 0;
  iCount := FStructure.SourceVectors.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xBar := _NewBar(xIndex);
      xCandel := FStructure.SourceVectors.Items[i];
      if i = (iCount - 1) then
        xCandel.Status := TTypeCandel.tcCurrent
      else
        xCandel.Status := TTypeCandel.tcSource;
      xBar.SetCandel(FMaxValue,FMinValue,xCandel);
      Inc(xIndex);
    end;


  iCount := FStructure.FutureVectors.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xBar := _NewBar(xIndex);
      xCandel := FStructure.FutureVectors.Items[i];
      if i = 0 then
        xCandel.Status := TTypeCandel.toLookingFor
      else
        xCandel.Status := TTypeCandel.tcFuture;
      xBar.SetCandel(FMaxValue,FMinValue,xCandel);
      Inc(xIndex);
    end;

end;

end.
