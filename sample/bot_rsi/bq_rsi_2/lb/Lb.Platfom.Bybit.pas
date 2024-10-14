unit Lb.Platfom.Bybit;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  Lb.SysUtils,
  Lb.Platfom;

type
  ///<summary>
  /// Соединение сервером Bybit
  ///</summary>
  TPlatfomBybit = class(TTradingPlatform)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TPlatfomBybit }

constructor TPlatfomBybit.Create;
begin
  inherited;

end;

destructor TPlatfomBybit.Destroy;
begin

  inherited;
end;

end.
