unit Lb.Element.DB;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  ///<summary>
  /// Тип лемента
  ///</summary>
  TTypeElemetn = (
    teNull,
    teDomain,
    teTable
  );

  TCustomElementObject = class;

  ///<summary>
  /// Базовый объект
  ///</summary>
  TCustomElementObject = class(TObject)
  private
    FName: String;
    FCode: String;
    FTypeElemetn: TTypeElemetn;
    FParent: TCustomElementObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Parent: TCustomElementObject read FParent write FParent;
    property Name: String read FName write FName;
    property Code: String read FCode write FCode;
    property TypeElemetn: TTypeElemetn read FTypeElemetn write FTypeElemetn;
  end;

  ///<summary>
  /// Список объектов
  ///</summary>
  TCustomElemetnObjectList = TObjectList<TCustomElementObject>;

  TElementDomain = class(TCustomElementObject)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TElementField = class(TCustomElementObject)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TElementTable = class(TCustomElementObject)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation


{ TCustomElementObject }

constructor TCustomElementObject.Create;
begin
  FParent := nil;
  FTypeElemetn := TTypeElemetn.teNull;
end;

destructor TCustomElementObject.Destroy;
begin

  inherited;
end;

{ TElementDomain }

constructor TElementDomain.Create;
begin
  inherited Create;
  FTypeElemetn := TTypeElemetn.teDomain;
end;

destructor TElementDomain.Destroy;
begin

  inherited;
end;

{ TElementField }

constructor TElementField.Create;
begin
  inherited;

end;

destructor TElementField.Destroy;
begin

  inherited;
end;

{ TElementTable }

constructor TElementTable.Create;
begin
  inherited Create;
  FTypeElemetn := TTypeElemetn.teTable;
end;

destructor TElementTable.Destroy;
begin

  inherited;
end;

end.
