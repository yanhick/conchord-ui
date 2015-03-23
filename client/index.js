import React from 'react';
import { HistoryLocation } from 'react-router';
import router from 'react-router';

import routes from './routes';

router.run(routes, HistoryLocation, function (Handler) {
    React.render(<Handler />, document.body);
});


