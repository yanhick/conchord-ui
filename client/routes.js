import React from 'react';
import Router from 'react-router';
import { Route, DefaultRoute } from 'react-router';

import App from './app';
import Home from './pages/home';

export default (
    <Route name="app" path="/" handler={App}>
        <DefaultRoute handler={Home} />
    </Route>
);
