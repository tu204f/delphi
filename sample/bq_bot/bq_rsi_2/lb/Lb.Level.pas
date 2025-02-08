unit Lb.Level;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  ///<summary>
  /// ��������� ������ ������������ ��������
  ///</summary>
  TStatusLevel = (slNull, slUp, slDown);

  ///<summary>
  /// ����������� �����
  ///  tlUpDown -
  ///</summary>
  TIntersectionLevel = (tlNull, tlUpDown, tlDownUp);

  ///<summary>
  /// ������ �������� �� ������ ������
  ///</summary>
  TLevel = class(TObject)
  private
    FValue: Double;
    FStatusLevel: TStatusLevel;
    FOldStatusLevel: TStatusLevel;
    FIntersectionLevel: TIntersectionLevel;
  private
    FOnIntersectionLevel: TNotifyEvent;
  protected
    procedure DoIntersectionLevel; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// ��������� �������� �� ��������� ������
    ///</summary>
    procedure SetUpDate(const ACurrentValue: Double);
    property Value: Double read FValue write FValue;
    property StatusLevel: TStatusLevel read FStatusLevel;
    property IntersectionLevel: TIntersectionLevel read FIntersectionLevel;
    property OnIntersectionLevel: TNotifyEvent write FOnIntersectionLevel;
  end;

  ///<summary>
  /// ������ �������
  ///</summary>
  TLevelList = TObjectList<TLevel>;

  ///<summary>
  /// � ������� ��������� � ��������� ������������
  ///</summary>
  TOneEventLevel = class(TLevel)
  private
    FRepeatCount: Integer;
    FCurrentRepeatCount: Integer;
    FIsRepeat: Boolean;
    FWorkLevel: TIntersectionLevel;
    procedure SetIsRepeat(const Value: Boolean);
    procedure SetRepeatCount(const Value: Integer);
  protected
    procedure DoIntersectionLevel; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>
    /// ���������� ����������
    ///</summary>
    property RepeatCount: Integer read FRepeatCount write SetRepeatCount;
    ///<summary>
    /// ����������� �� ������� ������������
    ///</summary>
    property IsRepeat: Boolean read FIsRepeat write SetIsRepeat;
    ///<summary>
    /// � ����� ����������� �������� �������
    ///</summary>
    property WorkLevel: TIntersectionLevel read FWorkLevel write FWorkLevel;
  end;



  ///<summary>
  /// ������ ��������� �������
  ///</summary>
  TOneEventLevelList = TObjectList<TOneEventLevel>;

  ///<summary>
  /// ������ ���������� ��������
  ///</summary>
  TLevelComparer = class(TComparer<TOneEventLevel>)
   function Compare(const Left, Right: TOneEventLevel): Integer; override;
  end;


function StatusLevelToStr(const AStatusLevel: TStatusLevel): String;
function IntersectionLevelToStr(const AIntersection: TIntersectionLevel): String;

implementation

function StatusLevelToStr(const AStatusLevel: TStatusLevel): String;
begin
  case AStatusLevel of
    slNull: Result := 'null';
    slUp: Result := 'up';
    slDown: Result := 'down';
  else
    raise Exception.Create('Error Message: ������ ������ - �� ���������');
  end;
end;

function IntersectionLevelToStr(const AIntersection: TIntersectionLevel): String;
begin
  case AIntersection of
    tlNull: Result := 'null';
    tlUpDown: Result := 'up_down';
    tlDownUp: Result := 'down_up';
  else
    raise Exception.Create('Error Message: ������ ����������� - �� ����������');
  end;
end;

{ TLevel }

constructor TLevel.Create;
begin
  FValue := -1;
  FStatusLevel := TStatusLevel.slNull;
  FOldStatusLevel := TStatusLevel.slNull;
  FIntersectionLevel := tlNull;
end;

destructor TLevel.Destroy;
begin

  inherited;
end;

procedure TLevel.DoIntersectionLevel;
begin
  if Assigned(FOnIntersectionLevel) then
    FOnIntersectionLevel(Self);
end;

procedure TLevel.SetUpDate(const ACurrentValue: Double);
begin
  if FValue < 0 then
    raise Exception.Create('Error Message: �������� ������ �� �����������');
  if ACurrentValue > FValue then
    FStatusLevel := slUp
  else
    FStatusLevel := slDown;

  if FStatusLevel <> FOldStatusLevel then
  begin
    if FOldStatusLevel in [slUp, slDown] then
    begin
      if (FStatusLevel = TStatusLevel.slUp) and (FOldStatusLevel = TStatusLevel.slDown) then
      begin
        FIntersectionLevel := TIntersectionLevel.tlDownUp;
        DoIntersectionLevel;
      end;
      if (FStatusLevel = TStatusLevel.slDown) and (FOldStatusLevel = TStatusLevel.slUp) then
      begin
        FIntersectionLevel := TIntersectionLevel.tlUpDown;
        DoIntersectionLevel;
      end;
    end;
    FOldStatusLevel := FStatusLevel;
  end;
end;

{ TOneEventLevel }

constructor TOneEventLevel.Create;
begin
  inherited Create;
  FIsRepeat := True;
  FWorkLevel := tlNull;
  FRepeatCount := 3;
  FCurrentRepeatCount := FRepeatCount;
end;

destructor TOneEventLevel.Destroy;
begin

  inherited;
end;

procedure TOneEventLevel.DoIntersectionLevel;
begin
  if FWorkLevel = tlNull then
    Exit;

  if FWorkLevel = FIntersectionLevel then
  begin
    if FIsRepeat then
      FCurrentRepeatCount := FCurrentRepeatCount - 1;

    if FCurrentRepeatCount > 0 then
    begin
      inherited DoIntersectionLevel;
    end
    else
    begin
      FIsRepeat := False;
      FWorkLevel := tlNull;
      inherited DoIntersectionLevel;
    end;
  end;
end;

procedure TOneEventLevel.SetIsRepeat(const Value: Boolean);
begin
  FIsRepeat := Value;
  if FIsRepeat then
    FCurrentRepeatCount := FRepeatCount
  else
    FCurrentRepeatCount := -1;
end;

procedure TOneEventLevel.SetRepeatCount(const Value: Integer);
begin
  FRepeatCount := Value;
  FCurrentRepeatCount := FRepeatCount;
end;

{ TLevelComparer }

function TLevelComparer.Compare(const Left, Right: TOneEventLevel): Integer;
begin
  // �� ����� ��� ��������� ���� ������, � �� ������ ����� ����� (
  //  0 � ������� �����,
  // -1 � ������ ������ �������,
  // 1 � ������ ������ �������
  if Left.Value = Right.Value then
    Result := 0
  else if Left.Value < Right.Value then
    Result := -1
  else
    Result := 1;
end;

end.
