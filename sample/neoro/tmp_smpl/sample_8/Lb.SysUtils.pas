unit Lb.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ILogger = interface
    procedure Log(S: String);
  end;

implementation

end.
