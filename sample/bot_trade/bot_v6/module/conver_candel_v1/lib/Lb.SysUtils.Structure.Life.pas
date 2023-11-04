(******************************************************************************)
(* Ѕазова€ форма - жизни (трейдер)
(*  ак работает имунна€ система - она начинает генерировать очень большое
(* количетво антител - что позвол€ет найти решение
(******************************************************************************)
unit Lb.SysUtils.Structure.Life;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Structure,
  Lb.SysUtils.Candel;

type
  ///<summary>„еловек которому нужны деньги</summary>
  TTrader = class(TObject)
  private
    FCapital: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Capital: Double read FCapital;
  end;

implementation

{ TTrader }

constructor TTrader.Create;
begin

end;

destructor TTrader.Destroy;
begin

  inherited;
end;

end.
