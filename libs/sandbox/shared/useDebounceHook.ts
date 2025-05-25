import { useEffect, useMemo } from 'react';
import debounce from 'lodash.debounce';

export function useDebouncedMutation<TArgs extends any[]>(
  mutationFn: (...args: TArgs) => void | Promise<void>,
  delay = 500
) {
  const debounced = useMemo(() => debounce(mutationFn, delay), [mutationFn, delay]);

  useEffect(() => {
    return () => {
      debounced.cancel();
    };
  }, [debounced]);

  return debounced;
}
