ex354:-otres([start imp x,
              x imp sometime p,
              neg x or neg p,
              x imp next y,
              neg y or neg p,
              neg y or t,
              t imp next neg p,
              t imp next t]).

ex5211:-otres([start imp x,
               x imp k y,
               y imp sometime p,
               x imp k neg p,
               x imp neg k neg z,
               z imp next w,
               neg w or neg p,
               neg w or t,
               t imp next neg p,
               t imp next t]).

ex5212:-otres([start imp x,
               x imp sometime y,
               y imp k p,
               x imp k neg p,
               x imp next z,
               neg z or w,
               neg z or t,
               t imp next w,
               t imp next t,
               w imp neg k p]).

ex621:-otres([start imp x,
              x imp k y,
              y imp next p,
              x imp next z,
              z imp neg k p]).

ex7121:-otres([start imp x,
               x imp next y,
               y imp k p,
               x imp neg k neg z,
               z imp next neg p]).

ex7122:-otres([start imp x,
               x imp next y,
               y imp k p,
               x imp neg k neg v,
               neg v or t,
               neg v or u,
               t imp next (w or neg p),
               u imp next (neg w or neg p)]).

ex7123:-otres([start imp x,
               neg x or z or y,
               neg x or z or w,
               w imp next (z or y),
               w imp next (z or w),
               x imp sometime z,
               z imp k p2,
               y imp k p1,
               x imp neg k neg r,
               neg r or t or s,
               neg r or t or u,
               u imp next (t or s),
               u imp next (t or u),
               t imp neg k p1,
               t imp neg k p2,
               s imp neg k p2]).

ex81:-otres([neg(k always p imp always k p)]).
ex82:-otres([neg(always k p imp k always p)]).
ex83:-otres([(k sometime p and k neg p) imp k next sometime p]).

ex84:-otres([start imp x,
             neg x or y,
             neg x or a,
             neg x or b,
             a and b imp next y,
             a imp next a,
             b imp next b,
             y imp k p,
             x imp neg k neg w,
             w imp sometime neg p]).

ex85:-otres([start imp x,
             neg x or y,
             neg x or a,
             neg x or b,
             a imp next y,
             b imp next y,
             a imp next a,
             b imp next b,
             y imp k p,
             x imp neg k neg w,
             w imp sometime neg p]).


ex86:-otres([start imp x,
             neg x or y,
             neg x or a,
             neg x or b,
             a imp next (c or y),
             b imp next (neg c or y),
             a imp next a,
             b imp next b,
             y imp k p,
             x imp neg k neg w,
             w imp sometime neg p]).

ex87:-otres([start imp y,
             neg y or a,
             neg y or b,
             neg y or l,
             x imp next l,
             a imp next l,
             b imp next (c or d),
             c imp next a,
             d imp next a,
             a imp next x,
             x imp next b,
             y imp sometime neg l]).

